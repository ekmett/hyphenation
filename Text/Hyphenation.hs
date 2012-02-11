-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation
-- Copyright   :  (C) 2012 Edward Kmett,
--                (C) 2007 Ned Batchelder
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Based on a Python implementation of Liang's algorithm placed in the
-- public domain by Ned Batchelder, July 2007. Simplified to remove the
-- manual exception case, by building patterns for the exceptions.
----------------------------------------------------------------------------
module Text.Hyphenation
  (
  -- * Hyphenate with a given set of patterns
    hyphenate
  -- * Pattern Files Support
  , readHyphenationPatterns
  -- ** Loading pattern Files from the installed data directory
  , hyphenateLanguage
  -- ** Supplied hyphenation pattern files
  , hyphenateEnglish
  , hyphenateFrench
  , hyphenateIcelandic
  ) where

import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.IntMap as IM
import Data.Char (isSpace, toLower)
import Paths_hyphenation
import System.IO.Unsafe

data Trie = Trie [Int] (IM.IntMap Trie)

insert :: String -> Trie -> Trie
insert s0 = go (chars s0) where
  go [] (Trie _ m) = Trie (points s0) m
  go (x:xs) (Trie n m) = Trie n (IM.insertWith (\_ -> go xs) (fromEnum x) (mk xs) m)

  mk []     = Trie (points s0) IM.empty
  mk (x:xs) = Trie [] (IM.singleton (fromEnum x) (mk xs))

points :: String -> [Int]
points (x:yzs@(_:zs))
  | x >= '0' && x <= '9' = (fromEnum x - fromEnum '0') : points zs
  | otherwise = 0 : points yzs
points [x] | x >= '0' && x <= '9' = [fromEnum x - fromEnum '0']
  | otherwise = [0,0]
points [] = [0]

chars :: String -> String
chars = filter (\x -> (x < '0' || x > '9'))

-- | Builds a hyphenator given a character normalization function
-- and a list of patterns.
--
-- Designed to be used partially applied to all but the last argument
hyphenate :: (Char -> Char) -> [String] 
          -> String -> [String]
hyphenate nf patterns = check where
  tree = foldr insert (Trie [] IM.empty) patterns
  check word
    | n <= 4 = [word]
    | otherwise = process [] word $ V.toList $ V.create $ do
      pts <- MV.replicate (n + 3) 0
      forM_ [0..n-1] $ walk pts tree
      MV.write pts 1 0
      MV.write pts 2 0
      MV.write pts n 0
      MV.write pts (n + 1) 0
      return $ MV.slice 2 n pts
    where
      process :: String -> String -> [Int] -> [String]
      process acc (w:ws) (p:ps)
         | odd p     = reverse (w:acc) : process [] ws ps
         | otherwise = process (w:acc) ws ps
      process acc [] [] = [reverse acc]
      process _ _ _ = error "hyphenate: the impossible happened"
      ls = map nf word
      work = V.fromList ('.' : ls ++ ".")
      n = length word
      walk allpts t i = step (V.toList (V.drop i work)) t
        where
          step (x:xs) (Trie _ m) = case IM.lookup (fromEnum x) m of
            Just t'@(Trie ps' _) -> do
               put i ps'
               step xs t'
            Nothing -> return ()
          step [] _ = return ()
          put j _ | j `seq` False = undefined
          put _ [] = return ()
          put j (x:xs) = do
            y <- MV.read allpts j
            MV.write allpts j $ max x y
            put (j + 1) xs

content :: String -> Bool
content (x:xs) = x /= '#' && not (isSpace x && content xs)
content _ = True

readHyphenationPatterns :: String -> IO [String]
readHyphenationPatterns fn = do
  body <- readFile fn
  return $ filter content (lines body) >>= words

hyphenateLanguage :: String -> IO (String -> [String])
hyphenateLanguage language = do
  src <- getDataFileName (language ++ ".hyp")
  patterns <- readHyphenationPatterns src
  return $ hyphenate toLower patterns

-- |
-- > ghci> hyphenateEnglish "supercalifragilisticexpialadocious"
-- ["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
hyphenateEnglish :: String -> [String]
hyphenateEnglish = unsafePerformIO (hyphenateLanguage "en")

-- |
-- > ghci> hyphenateFrench "anticonstitutionnellement"
-- > ["an","ti","cons","ti","tu","tion","nel","le","ment"]
hyphenateFrench :: String -> [String]
hyphenateFrench = unsafePerformIO (hyphenateLanguage "fr")

-- |
-- > ghci> hyphenateIcelandic "vaðlaheiðavegavinnuverkfærageymsluskúr"
-- > ["va\240la","hei\240a","vega","vinnu","verk","f\230ra","geymslu","sk\250r"]
hyphenateIcelandic :: String -> [String]
hyphenateIcelandic = unsafePerformIO (hyphenateLanguage "is")
