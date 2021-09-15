{-# LANGUAGE OverloadedStrings #-}

module Router where

import Data.List (find)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, status400, status404)
import Network.HTTP.Types.Header (Header)

import qualified Logger

type Path = Text

data Method = GET | POST | PUT | DELETE | UNDEFINED deriving (Eq, Show, Read, Enum)

data Route = Route Method Path Wai.Application

type Router = [Route]

-- bytestring always shows with quotes, so this should remove them
parseMethod :: Wai.Request -> Method
parseMethod req
    | length r == 0 = UNDEFINED
    | e /= "" = UNDEFINED
    | otherwise = m
    where
        s = ByteString.unpack $ Wai.requestMethod req
        r = reads s :: [(Method, String)]
        [(m, e)] = r

-- (request seg, template seg) -> boolean
pathSegsEqual :: (Text, Text) -> Bool
pathSegsEqual p
    | rs == ts = True
    | ts == "" = False
    | otherwise = Text.head ts == ':'
    where
        (rs, ts) = p

-- request path -> template path -> boolean
pathsEqual :: Path -> Path -> Bool
pathsEqual rp tp
    | length rps /= length tps = False
    | otherwise = all pathSegsEqual pz
    where
        rps = Text.split (== '/') rp
        tps = Text.split (== '/') tp
        pz = zip rps tps

compareRoute :: Method -> Path -> Route -> Bool
compareRoute reqm reqp rt =
    let (Route m p _) = rt
    in m == reqm && pathsEqual reqp p

contentJSON :: Header
contentJSON = ("Content-Type", "application/json")

contentText :: Header
contentText = ("Content-Type", "text/plain")

router :: Router -> Wai.Application
router rt req res = do
    Logger.request req
    let m = parseMethod req
    let p = Text.pack $ ByteString.unpack $ Wai.rawPathInfo req
    let f = find (compareRoute m p) rt
    case f of
        Nothing -> do
            responseReceived <- res $ Wai.responseBuilder status404 [contentText] "Route Not Found"
            return responseReceived
        Just r -> do
            let (Route _ _ app) = r
            app req res

-- parsing requests
type QueryMap = Map String [String]

digestQuery :: String -> QueryMap
digestQuery s
    | length s == 0 = Map.empty
    | otherwise = Map.insert k (v:fv) m
    where
        kv = takeWhile (/= '&') s
        rs = drop (length kv + 1) s
        k = takeWhile (/= '=') kv
        v = drop (length k + 1) kv
        m = digestQuery rs
        fv = maybe [] id $ Map.lookup k m

parseQuery :: Wai.Request -> QueryMap
parseQuery = digestQuery . tail . ByteString.unpack . Wai.rawQueryString
