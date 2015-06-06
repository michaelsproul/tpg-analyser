{-# LANGUAGE OverloadedStrings #-}

module Data.TPG where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isSpace)
import Data.String (fromString)
import Data.List (stripPrefix, isSuffixOf, elemIndex)
import Text.Printf (printf)
import Data.Maybe (fromJust)

import Control.Lens
import Control.Arrow

import Network.Wreq
import Network.Wreq.Types
import qualified Network.Wreq.Session as S

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlArrow (getAttrValue)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Tree.NTree.TypeDefs (NTree(..))
import Text.HandsomeSoup

baseUrl = "https://cyberstore.tpg.com.au/your_account/"
indexUrl = baseUrl ++ "index.php"

-- Type describing a month of billing.
data BillingPeriod = BillingPeriod String Int deriving (Show)

-- Type describing a single charge/cost.
-- Charge (time of charge) (plan cost in cents) (excess cost in cents) (info)
data Charge = Charge String Int Int ChargeInfo deriving (Show)

-- Type describing the reason for a charge.
data ChargeInfo =
    --- Sms (number)
    Sms String |
    -- Call (type) (number) (duration in seconds)
    Call CallType String Int |
    -- Data (usage in kB)
    Data Int |
    -- Voicemail (number) (duration in seconds)
    Voicemail String Int |
    -- Misc
    Other String deriving (Show)

data CallType = Mobile | Landline | Tpg deriving (Show)

-- Login to the TPG user panel.
login :: String -> String -> IO (S.Session)
login username password = S.withSession $ \sesh -> do
    -- Login.
    let loginData = ["check_username" := username,
                    "password" := password,
                    "password1" := ("Password" :: B.ByteString),
                    "x" := (0 :: Int),
                    "y" := (0 :: Int) ]

    S.post sesh indexUrl loginData
    return sesh

getBillingPeriods :: S.Session -> Int -> IO [BillingPeriod]
getBillingPeriods sesh planId = do
    index <- downloadIndex sesh planId
    parseIndex index planId

-- Download the page that lists billing periods.
downloadIndex :: S.Session -> Int -> IO (Response LB.ByteString)
downloadIndex sesh planId = do
    -- Query params.
    let opts = defaults & param "function" .~ ["view_all_mobile"]
    -- POST data.
    let planSelection = B.append "viewdetails-" (fromString (show planId))

    S.postWith opts sesh indexUrl (planSelection := ("Mobile+Usage" :: B.ByteString))

-- Parse the index page to form a list of "billing periods".
parseIndex :: Response LB.ByteString -> Int -> IO [BillingPeriod]
parseIndex response planId = do
    nodes <- runX $ doc >>> css selector >>> (getInnerText &&& getAttrValue "href")
    return (map createBillingPeriod nodes)
    where
        doc = responseToDoc response
        linkPrefix = getLinkPrefix planId
        selector = printf "a[href|=\"%s\"]" linkPrefix
        createBillingPeriod (title, chargeId) = BillingPeriod title (createChargeId chargeId)
        createChargeId = read . fromJust . (stripPrefix linkPrefix)


-- Get the string that appears at the beginning of all links for a given plan ID.
getLinkPrefix :: Int -> String
getLinkPrefix planId = printf "index.php?function=view_all_mobile&plan_id=%s&chg_id=" (show planId)

-- Get the charge data for a single billing period.
getDataForPeriod :: S.Session -> Int -> BillingPeriod -> IO [Charge]
getDataForPeriod sesh planId (BillingPeriod desc chargeId) = do
    page <- S.get sesh pageUrl
    let doc = responseToDoc page
    cells <- runX $ doc >>> css "table[rules=all] tr td" >>> getInnerText
    let rows = splitEvery 6 cells
    return (map (rowToCharge . map trim) rows)
    where
        pageUrl = baseUrl ++ getLinkPrefix planId ++ show chargeId

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n l = start : (splitEvery n rest)
    where (start, rest) = splitAt n l

-- Inefficient trim function.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

getInnerText :: Arrow a => a XmlTree String
getInnerText = arr (\(NTree _ (child:_)) -> fromJust (XN.getText child))

-- Convert a response to an XML document.
responseToDoc :: Response LB.ByteString -> IOSArrow b (NTree XNode)
responseToDoc res = (parseHtml . LB.unpack) (res ^. responseBody)

-- Parse a cost like $0.00 to a value in cents.
parseCost :: String -> Int
parseCost ('$':x) = round ((read x :: Float) * 100)

-- Parse a data value like 1.50MB to an integer value in decimal kilobytes.
parseDataUse :: String -> Int
parseDataUse s = round ((read mb :: Float) * 1000)
    where
        mb = if "MB" `isSuffixOf` s then take (length s - 2) s else error "MB suffix not found"

-- Parse a duration in minutes like 10:15 to a duration in seconds.
parseDuration :: String -> Int
parseDuration s = (read minutes) * 60 + read seconds
    where
        colonIdx = fromJust (elemIndex ':' s)
        (minutes, ':':seconds) = splitAt colonIdx s

parseCallType :: String -> Maybe CallType
parseCallType "Call to landline" = Just Landline
parseCallType "Info Service" = Just Landline
parseCallType "Mobile Call" = Just Mobile
parseCallType "Call to non-Optus GSM" = Just Mobile
parseCallType "TPG Mobile to TPG Mobile" = Just Tpg
parseCallType _ = Nothing

-- Convert a row of a charge table into a charge.
rowToCharge :: [String] -> Charge
rowToCharge [time, ty, value, number, planCost, excessCost] =
    Charge time (parseCost planCost) (parseCost excessCost) (createChargeInfo ty value number)

createChargeInfo :: String -> String -> String -> ChargeInfo
createChargeInfo "SMS National" _ number = Sms number
createChargeInfo v duration number
    | v `elem` ["Voicemail Deposit", "Voicemail Retrieval"] = Voicemail number (parseDuration duration)
createChargeInfo d mb _
    | d `elem` ["Data", "Data (Volume based)"] = Data $ parseDataUse mb
createChargeInfo callType duration number =
    case parseCallType callType of
        Just ty -> Call ty number (parseDuration duration)
        Nothing -> Other callType

testSingle :: String -> String -> Int -> Int -> IO (BillingPeriod, [Charge])
testSingle username password planId i = do
    sesh <- login username password
    bps <- getBillingPeriods sesh planId
    let bp = bps !! i
    charges <- getDataForPeriod sesh planId bp
    return (bp, charges)

testAll :: String -> String -> Int -> IO [(BillingPeriod, [Charge])]
testAll username password planId = do
    sesh <- login username password
    bps <- getBillingPeriods sesh planId
    charges <- mapM (getDataForPeriod sesh planId) bps
    return (zip bps charges)
