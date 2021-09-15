{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO

import Data.Char (toUpper, toLower)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import qualified Data.ByteString.Lazy as ByteLazy
import qualified Data.ByteString.Builder as ByteBuilder

import qualified Data.Aeson as Aeson

import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (runEnv)

import qualified Autocomplete as Auto
import qualified Logger as Log

import qualified Router
import Router ( Method(GET)
              , contentJSON
              , contentText
              , parseQuery
              )

type ColorMap = Map String String

-- search options
opts :: Auto.SearchOptions
opts = Auto.defaultSearchOptions

-- -- application start
main :: IO ()
main = do
    ls <- ByteLazy.readFile "assets/colors.json"
    let dm = Aeson.decode ls :: Maybe ColorMap
    let m = maybe Map.empty id dm
    let trie = Auto.fromList $ Map.keys m
    let r = [Router.Route GET "/search" (autocompleteRoute trie m)]
    runEnv 3000 $ Router.rout r

autocompleteRoute :: Auto.Trie -> ColorMap -> Wai.Application
autocompleteRoute trie cm req res = do
    let qm = parseQuery req
    let ks = Map.keys qm
    let sv = maybe [""] id $ Map.lookup "chars" qm
    if (length ks /= 1 || head ks /= "chars" || length sv > 1)
        then do
            responseReceived <- res $ Wai.responseBuilder status400 [contentText] "Invalid query params"
            return responseReceived
        else do
            let rs = Aeson.encode $ Auto.search trie opts $ head sv
            let js = ByteBuilder.byteString $ ByteLazy.toStrict rs
            responseReceived <- res $ Wai.responseBuilder status200 [contentJSON] js
            return responseReceived
