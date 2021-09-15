{-# LANGUAGE OverloadedStrings #-}

module GetColor where

import System.IO

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.List as List

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as ByteLazy
import qualified Data.ByteString.Builder as ByteBuilder

import qualified Data.Aeson as Aeson

import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, status400)

import Autocomplete (Trie)
import qualified Autocomplete as Auto

import qualified Router
import Router ( Method(GET)
              , contentJSON
              , contentText
              , parseQuery
              )

type ColorMap = Map Text Text
type ColorItem = (Text, Text)

-- search options
opts :: Auto.SearchOptions
opts = Auto.defaultSearchOptions

createColorTrie :: IO (Trie, ColorMap)
createColorTrie = do
    ls <- ByteLazy.readFile "assets/colors.json"
    let dl = Aeson.decode ls :: Maybe [ColorItem]
    let l = maybe [] id dl
    let m = foldl (\a c -> Map.insert (fst c) (snd c) a) Map.empty l
    let t = Auto.fromList $ Map.keys m
    return (t, m)

-- routes
handle :: Trie -> ColorMap -> Wai.Application
handle trie cm req res = do
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

route :: Trie -> ColorMap -> Router.Route
route trie mp = Router.Route GET "/search" (handle trie mp)
