{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (runEnv)

import qualified Logger as Log
import qualified Router

-- routes
import qualified GetColor

-- -- application start
main :: IO ()
main = do
    (trie, mp) <- GetColor.createColorTrie
    let r = [GetColor.route trie mp]
    runEnv 3000 $ Router.rout r
