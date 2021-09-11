module Autocomplete where

import Data.Maybe (catMaybes)

import Data.Map (Map)
import qualified Data.Map as Map

type TrieMap = Map Char Trie

data Trie = Trie
    { char :: Char
    , key :: Bool
    , children :: TrieMap
    }

-- creates empty trie
empty :: Trie
empty = Trie
    { char = '_'
    , key = False
    , children = Map.empty :: TrieMap
    }

-- creates a trie given a character and key value
new :: Char -> Bool -> Trie
new c k = Trie
    { char = c
    , key = k
    , children = Map.empty :: TrieMap
    }

-- creates a trie from a list
fromList :: [String] -> Trie
fromList ls = foldl (\t v -> update t v) empty ls

-- updates a trie with given children
replaceChildren :: Trie -> TrieMap -> Trie
replaceChildren t m = Trie
    { char = (char t)
    , key = (key t)
    , children = m
    }

-- updates a trie with a new string
update :: Trie -> String -> Trie
update t s
    | s == "" = t
    | otherwise = replaceChildren t nm
    where
        c = head s
        cm = children t
        d = new c $ length s == 1
        nt = maybe d (\v -> v) $ Map.lookup c cm
        nm = Map.insert c (update nt (tail s)) cm

-- checks if string is contained in trie
contains :: Trie -> String -> Bool
contains t s
    | s == "" = key t
    | otherwise = maybe False (\v -> contains v (tail s)) nt
    where
        c = head s
        nt = Map.lookup c $ children t

-- helper function for retrieving the next set of node elems
findElems :: Trie -> [Trie]
findElems t = Map.elems $ children t

-- helper function for search, retrieves all keyed values that are decendants of a given trie
getKeyedFull :: [String] -> Maybe Char -> [(String, Trie)] -> String -> Int -> [String]
getKeyedFull rs ex ts sc n
    | n == 0 = rs
    | length ts == 0 = rs
    | otherwise = getKeyedFull nrs Nothing fs (tail sc) m
    where
        (s, t) = head ts
        cs = s ++ [char t]
        k = key t
        nrs = if (key t) then rs ++ [cs] else rs
        m = n - fromEnum k
        ns = map (\v -> (cs, v)) $ findElems t
        fs = (tail ts) ++ ns

-- getKeyedFull hofs
getKeyed :: Maybe Char -> [(String, Trie)] -> String -> Int -> [String]
getKeyed = getKeyedFull []

getKeyedAll :: [(String, Trie)] -> String -> Int -> [String]
getKeyedAll = getKeyedFull [] Nothing

-- helper function for search, adds all nodes to a stack along given string path
findNodePath :: Trie -> String -> [Trie]
findNodePath t s
    | s == "" = []
    | otherwise = case nt of
        Nothing -> []
        Just t2 -> (findNodePath t2 tl) ++ [t2]
    where
        c = head s
        tl = tail s
        nt = Map.lookup c $ children t

-- searches trie for given string
search :: Int -> Trie -> String -> [String]
search n t s
    | ns < 1 = []
    -- 0.7 is the ratio between node path length and search string length,
    -- used to determine when to run a search
    | fromIntegral ns / fromIntegral (length s) < 0.7 = []
    | otherwise = getKeyedAll [(str, head ts)] sc n
    where
        ts = findNodePath t s
        ns = length ts
        str = take (ns - 1) s
        sc = drop ns s
