{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List

import Data.List (intersperse)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Network.Wai
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (runEnv)

import qualified Autocomplete as Auto
import qualified Logger as Log
import qualified JSONBuilder as JSON

-- search options
opts :: Auto.SearchOptions
opts = Auto.defaultSearchOptions

-- type Param = (ByteString, Maybe ByteString)

-- key :: Param -> ByteString
-- key p = let (k, _) = p in k

-- val :: Param -> ByteString
-- val p = let (_, v) = p in maybe ByteString.empty id v

-- getQueryParamVal :: ByteString -> [Param] -> [ByteString]
-- getQueryParamVal k bs =
--     let p = List.filter (\v -> key v == k) bs
--     in map val p

-- application definition
application :: Auto.Trie -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application autoref req res = do
    Log.logRequest req
    -- let search = ByteString.unwords $ getQueryParamVal "search" $ queryString req
    -- if (length search /= 1)
    --     then do
    --         let msg = JSON.errorMsg "'search' query param required"
    --         responseReceived <- res $ responseBuilder
    --             status400
    --             [("Content-Type", "application/json")]
    --             msg
    --         return responseReceived
    --     else do
    let rs = Auto.search autoref opts "ho"
    let msg = JSON.build $ JSON.strArrayify ByteString.pack rs
    responseReceived <- res $ responseBuilder status200 [("Content-Type", "application/json")] msg
    return responseReceived

-- -- application start
main :: IO ()
main = do
    let auto = Auto.fromList ["hat", "hills", "holy", "hola", "ho ho ho", "hobo"]
    runEnv 3000 $ application auto
    -- return ()
