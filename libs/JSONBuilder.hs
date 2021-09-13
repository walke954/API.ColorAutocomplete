{-# LANGUAGE OverloadedStrings #-}

module JSONBuilder where

import Data.ByteString.Builder (byteString, Builder)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

type KeyVal k v = (k, v)

-- adds quotes to input
bStringify :: ByteString -> ByteString
bStringify s = ByteString.concat ["\"", s, "\""]

stringify :: (Show a) => (a -> ByteString) -> a -> ByteString
stringify tf = bStringify . tf

-- adds commas between items
bCommafy :: [ByteString] -> ByteString
bCommafy = ByteString.intercalate ","

commafy :: (Show a) => (a -> ByteString) -> [a] -> ByteString
commafy tf = bCommafy . map tf

-- adds square brackets around input
bSqBracket :: ByteString -> ByteString
bSqBracket s = ByteString.concat ["[", s, "]"]

sqBracket :: (Show a) => (a -> ByteString) -> a -> ByteString
sqBracket tf = bSqBracket . tf

-- adds curly brackets around input
bCBracket :: ByteString -> ByteString
bCBracket s = ByteString.concat ["{", s, "}"]

cBracket :: (Show a) => (a -> ByteString) -> a -> ByteString
cBracket tf = bCBracket . tf

-- combines bSqBracket and bCommafy
bArrayify :: [ByteString] -> ByteString
bArrayify = bSqBracket . bCommafy

arrayify :: (Show a) => (a -> ByteString) -> [a] -> ByteString
arrayify tf = bArrayify . map tf

-- variation on bArrayify that uses bStringify on each item
bStrArrayify :: [ByteString] -> ByteString
bStrArrayify = bSqBracket . bCommafy . map bStringify

strArrayify :: (Show a) => (a -> ByteString) -> [a] -> ByteString
strArrayify tf = bStrArrayify . map tf

-- bEntryify :: KeyVal ByteString ByteString -> ByteString
-- bEntryify kv =
--     let (k, v) = kv
--     in ByteString.concat [bStringify k, v]

-- entryify :: (Show a) => (a -> ByteString) -> KeyVal ByteString a -> ByteString
-- entryify tf kv =
--     let (k, v) = kv
--     in bEntryify $ (k, tf v)

-- bObjectify :: [KeyVal ByteString a] -> ByteString
-- bObjectify tf ls =
--     let tls = map (entryify tf) ls
--     in ByteString.concat ["{", ByteString.intercalate "," tls, "}"]

-- objectify :: (Show a) => (a -> ByteString) -> [KeyVal ByteString a] -> ByteString
-- objectify tf ls =
--     let tls = map (entryify tf) ls
--     in ByteString.concat ["{", ByteString.intercalate "," tls, "}"]

build :: ByteString -> Builder
build bs = byteString bs
-- fromStringList :: Transformer -> [ByteString] -> Builder
-- fromStringList ls = byteString $ arrayify $ map stringify ls

-- errorMsg :: ByteString -> Builder
-- errorMsg