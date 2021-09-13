{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Data.Time
import Data.Time.Format

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Data.ByteString.Char8 (pack)

import System.IO

import Network.Wai

isoTime :: UTCTime -> ByteString
isoTime = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"

logRequest :: Request -> IO ()
logRequest req = do
    let method = requestMethod req
    let pathquery = ByteString.concat [rawPathInfo req, rawQueryString req]
    time <- getCurrentTime
    ByteString.putStrLn $ ByteString.concat ["[", (isoTime time), "] ", method, " ", pathquery]
