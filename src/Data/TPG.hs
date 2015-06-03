{-# LANGUAGE OverloadedStrings #-}

module Data.TPG where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.String (fromString)
import Data.List (stripPrefix)
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

baseUrl = "https://cyberstore.tpg.com.au/your_account/index.php"

data BillingPeriod = BillingPeriod String Int deriving (Show)

-- Login to the TPG user panel.
login :: String -> String -> IO (S.Session)
login username password = S.withSession $ \sesh -> do
    -- Login.
    let loginData = ["check_username" := username,
                    "password" := password,
                    "password1" := ("Password" :: B.ByteString),
                    "x" := (0 :: Int),
                    "y" := (0 :: Int) ]

    S.postWith defaults sesh baseUrl loginData
    return sesh

-- Download the page that lists billing periods.
downloadIndex :: S.Session -> Int -> IO (Response LB.ByteString)
downloadIndex sesh planId = do
    -- Query params.
    let opts = defaults & param "function" .~ ["view_all_mobile"]
    -- POST data.
    let planSelection = B.append "viewdetails-" (fromString (show planId))

    S.postWith opts sesh baseUrl (planSelection := ("Mobile+Usage" :: B.ByteString))

-- Parse the index page to form a list of "billing periods".
parseIndex :: LB.ByteString -> Int -> IO [BillingPeriod]
parseIndex page planId = do
    nodes <- runX $ doc >>> css selector >>> (getInnerText &&& getAttrValue "href")
    return (map createBillingPeriod nodes)
    where
        doc = (parseHtml . LB.unpack) page
        linkPrefix = printf "index.php?function=view_all_mobile&plan_id=%s&chg_id=" (show planId) :: String
        selector = printf "a[href|=\"%s\"]" linkPrefix :: String
        getInnerText = arr (\(NTree _ (child:_)) -> fromJust (XN.getText child))
        createBillingPeriod (title, chargeId) = BillingPeriod title (createChargeId chargeId)
        createChargeId = read . fromJust . (stripPrefix linkPrefix)

test :: String -> String -> Int -> IO [BillingPeriod]
test username password planId = do
    sesh <- login username password
    index <- downloadIndex sesh planId
    parseIndex (index ^. responseBody) planId
