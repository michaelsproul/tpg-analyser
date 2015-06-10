{-# LANGUAGE OverloadedStrings, ViewPatterns, DeriveGeneric #-}

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

import Data.Binary
import GHC.Generics (Generic)

baseUrl = "https://cyberstore.tpg.com.au/your_account/"
indexUrl = baseUrl ++ "index.php"

-- Type describing a month of billing.
data BillingPeriod = BillingPeriod String Int deriving (Show, Generic)

-- Type describing a single charge/cost.
-- Charge (time of charge) (plan cost in cents) (excess cost in cents) (info)
data Charge = Charge {
    time :: String,
    planCost :: Int,
    excessCost :: Int,
    chargeInfo :: ChargeInfo
} deriving (Show, Generic)

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
    Other String deriving (Show, Generic)

data CallType = Mobile | Landline | Tpg deriving (Show, Generic)

-- Serialisation instances.
instance Binary BillingPeriod
instance Binary Charge
instance Binary ChargeInfo
instance Binary CallType

isSms (chargeInfo -> Sms _) = True
isSms _ = False

isCall (chargeInfo -> Call _ _ _) = True
isCall _ = False

isMobileCall (chargeInfo -> Call Mobile _ _) = True
isMobileCall _ = False

isLandlineCall (chargeInfo -> Call Landline _ _) = True
isLandlineCall _ = False

isTpgCall (chargeInfo -> Call Tpg _ _) = True
isTpgCall _ = False

isData (chargeInfo -> Data _) = True
isData _ = False

isVoicemail (chargeInfo -> Voicemail _ _) = True
isVoicemail _ = False

isOther (chargeInfo -> Other _) = True
isOther _ = False

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

-- Fetch the list of billing periods for a given plan ID.
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

-- Parse the index page to form a list of billing periods.
parseIndex :: Response LB.ByteString -> Int -> IO [BillingPeriod]
parseIndex response planId = do
    nodes <- runX $ doc >>> css selector >>> (arr getInnerText &&& getAttrValue "href")
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
    rows <- runX $ doc >>> css "table[rules=all] tr" >>> arr createRow
    return (map rowToCharge rows)
    where
        pageUrl = baseUrl ++ getLinkPrefix planId ++ show chargeId
        -- Extract the string contents of an XML <tr> node's children.
        createRow (NTree _ children) = map (trim . getInnerText) children

-- Fetch the text from within an XML node.
getInnerText :: NTree XNode -> String
getInnerText (NTree _ (inner:_)) = fromJust (XN.getText inner)

-- Convert a response to an XML document.
responseToDoc :: Response LB.ByteString -> IOSArrow b (NTree XNode)
responseToDoc res = (parseHtml . LB.unpack) (res ^. responseBody)

-- Parse a cost like $0.00 to a value in cents.
parseCost :: String -> Int
parseCost ('$':x) = round ((read x :: Float) * 100)

-- Parse a data value like 1.50MB to an integer value in decimal kilobytes.
parseDataUse :: String -> Int
parseDataUse s = round (mb * 1000)
    where mb = (read . fromJust . stripSuffix "MB") s

-- Parse a duration in minutes like 10:15 to a duration in seconds.
parseDuration :: String -> Int
parseDuration s = (read minutes) * 60 + read seconds
    where
        colonIdx = fromJust (elemIndex ':' s)
        (minutes, ':':seconds) = splitAt colonIdx s

-- Parse a call type string.
parseCallType :: String -> Maybe CallType
parseCallType "Call to landline" = Just Landline
parseCallType "Info Service" = Just Landline
parseCallType "Mobile Call" = Just Mobile
parseCallType "Call to non-Optus GSM" = Just Mobile
parseCallType "TPG Mobile to TPG Mobile" = Just Tpg
parseCallType _ = Nothing

-- Convert a row of a charge table into a charge.
rowToCharge :: [String] -> Charge
-- Pay as you go format (6 columns).
rowToCharge [time, ty, value, number, planCost, excessCost] =
    Charge time (parseCost planCost) (parseCost excessCost) (createChargeInfo ty value number)
rowToCharge _ = Charge "" 0 0 $ Other "irregular row - doesn't have an understood number of columns"

createChargeInfo :: String -> String -> String -> ChargeInfo
createChargeInfo "SMS National" _ number = Sms number
createChargeInfo v duration number
    | v `elem` ["Voicemail Deposit", "Voicemail Retrieval"] =
    Voicemail number (parseDuration duration)
createChargeInfo d mb _
    | d `elem` ["Data", "Data (Volume based)"] = Data $ parseDataUse mb
createChargeInfo (parseCallType -> Just ty) duration number =
    Call ty number (parseDuration duration)
createChargeInfo desc _ _ = Other desc

-- Download the charges for a single (numbered) billing period.
downloadSingle :: String -> String -> Int -> Int -> IO (BillingPeriod, [Charge])
downloadSingle username password planId i = do
    sesh <- login username password
    bps <- getBillingPeriods sesh planId
    let bp = bps !! i
    charges <- getDataForPeriod sesh planId bp
    return (bp, charges)

-- Download all the charges for a given plan ID.
downloadAll :: String -> String -> Int -> IO [(BillingPeriod, [Charge])]
downloadAll username password planId = do
    sesh <- login username password
    bps <- getBillingPeriods sesh planId
    charges <- mapM (getDataForPeriod sesh planId) bps
    return (zip bps charges)

totalBy :: (Charge -> Int) -> (Charge -> Bool) -> [[Charge]] -> [Int]
totalBy cost p charges = map (sum . map cost . filter p) charges

avg :: [Int] -> Double
avg l = fromIntegral (sum l) / fromIntegral (length l)

-- Inefficient trim function.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- Remove a suffix from a list.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s l = if s `isSuffixOf` l then Just $ take (length l - length s) l else Nothing
