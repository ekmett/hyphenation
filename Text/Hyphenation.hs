-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation
-- Copyright   :  (C) 2012 Edward Kmett,
--                (C) 2007 Ned Batchelder
--                (C) 1990, 2004, 2005 Gerard D.C. Kuiken
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Based on a Python implementation of Liang's algorithm placed in the
-- public domain by Ned Batchelder, July 2007.
----------------------------------------------------------------------------
module Text.Hyphenation
  (
  -- * Extended API
    hyphenate
  , readPatterns
  ) where

import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.IntMap as IM
import Data.Char (isSpace)

data Trie = Trie [Int] (IM.IntMap Trie)

instance Show Trie where
  showsPrec d (Trie xs ys) = showParen (d > 10) $
    showString "Trie " . showsPrec 11 xs . showChar ' ' . showsPrec 11 (map (\(k,v) -> (toEnum k :: Char, v)) (IM.toList ys))

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

-- | Build a hyphenator, given a character normalization function
-- and a list of patterns.
--
-- Designed to be used partially applied.
--
-- > hyphenate_EN = hyphenate toLower englishPatterns
hyphenate :: (Char -> Char) -> [String] -> String -> [String]
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
      process _ _ _ = error "hyphenate': the impossible happened"
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

comment :: String -> Bool
comment (x:xs) = x == '#' || (isSpace x && comment xs)
comment _ = False

readPatterns :: String -> IO [String]
readPatterns fn = do
  content <- readFile fn
  return $ filter (not . comment) (lines content) >>= words
