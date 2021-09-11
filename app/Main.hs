module Main where

import qualified Autocomplete as Auto

testList :: [String]
testList = ["hello", "goodbye", "godspeed", "good-looking", "gassy", "greeeeat!"]

main :: IO ()
main = do
    let t = Auto.fromList testList
    mainLoop t

mainLoop :: Auto.Trie -> IO ()
mainLoop t = do
    str <- getLine
    let ls = Auto.search 10 t str
    putStrLn $ unwords ls
    mainLoop t
