{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO

import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (runEnv)

import qualified Autocomplete as Auto
import qualified Logger as Log
import qualified JSONBuilder as JSONB

import Router

-- search options
opts :: Auto.SearchOptions
opts = Auto.defaultSearchOptions

-- -- application start
main :: IO ()
main = do
    handle <- openFile "assets/test.txt" ReadMode  
    ls <- hGetContents handle
    let trie = Auto.fromList $ words ls
    let r = [Route GET "/search" (autocompleteRoute trie)]
    runEnv 3000 $ router r

autocompleteRoute :: Auto.Trie -> Wai.Application
autocompleteRoute trie req res = do
    let qm = parseQuery req
    let ks = Map.keys qm
    let sv = maybe [""] id $ Map.lookup "chars" qm
    if (length ks /= 1 || head ks /= "chars")
        then do
            responseReceived <- res $ Wai.responseBuilder status400 [contentText] "Invalid query params"
            return responseReceived
        else do
            let rs = Auto.search trie opts $ head sv
            let js = JSONB.build $ JSONB.strArrayify ByteString.pack rs
            responseReceived <- res $ Wai.responseBuilder status200 [contentJSON] js
            return responseReceived
