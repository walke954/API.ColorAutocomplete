module Main where

import qualified Autocomplete as Auto

testList :: [String]
testList = ["hello", "goodbye", "godspeed", "good-looking", "gassy", "greeeeat!"]

opts :: Auto.SearchOptions
opts = Auto.defaultSearchOptions

main :: IO ()
main = do
    let t = Auto.fromList testList
    mainLoop t

mainLoop :: Auto.Trie -> IO ()
mainLoop t = do
    str <- getLine
    let ls = Auto.search t Auto.defaultSearchOptions str
    putStrLn $ unwords ls
    mainLoop t
