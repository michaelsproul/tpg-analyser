{-# LANGUAGE OverloadedStrings #-}

module Data.TPG where

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.String

import Network.Wreq
import Network.Wreq.Types
import qualified Network.Wreq.Session as S

baseUrl = "https://cyberstore.tpg.com.au/your_account/index.php"

getPage :: String -> String -> Int -> IO (Response LB.ByteString)
getPage username password planId = S.withSession $ \sesh -> do
    -- Login.
    let loginData = ["check_username" := username,
                    "password" := password,
                    "password1" := ("Password" :: B.ByteString),
                    "x" := (66 :: Int),
                    "y" := (16 :: Int) ]

    S.postWith defaults sesh baseUrl loginData

    -- Retrieve the page for the desired plan.
    let opts = defaults & param "function" .~ ["view_all_mobile"]
    let planSelection = B.append "viewdetails-" (fromString (show planId))

    S.postWith opts sesh baseUrl (planSelection := ("Mobile+Usage" :: B.ByteString))
