{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Pattern
-- Copyright   :  (C) 2012-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Text.Hyphenation.Pattern
  (
  -- * Pattern file support
    Patterns
  , insertPattern
  , lookupPattern
  , scorePattern
  , parsePatterns
  ) where

import qualified Data.IntMap as IM
import Prelude hiding (lookup)
import Data.Char (digitToInt, isDigit)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- | Hyphenation patterns
data Patterns = Patterns [Int] (IM.IntMap Patterns)
  deriving Show

instance Semigroup Patterns where
  Patterns ps m <> Patterns qs n = Patterns (zipMax ps qs) (IM.unionWith mappend m n)

instance Monoid Patterns where
  mempty = Patterns [] IM.empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

-- | Tallies the hyphenation scores for a word considering all tails.
lookupPattern :: String -> Patterns -> [Int]
lookupPattern xs0 = init . tail . go ('.' : xs0 ++ ".") where
  go [] (Patterns ys _) = ys
  go xxs@(_:xs) t = zipMax (go1 xxs t) (0:go xs t)
  go1 [] (Patterns ys _) = ys
  go1 (x:xs) (Patterns ys m) = case IM.lookup (fromEnum x) m of
    Just t' -> zipMax ys (go1 xs t')
    Nothing -> ys

-- | Insert a Knuth-Liang hyphenation pattern into the trie
--
-- 1. @.@ denotes the start or end of the input
--
-- 2. @0-9@ are used to denote hyphenation or dehyphenation depending on whether or not they are even (no hyphen) or odd (hyphen allowed).
--
-- Patterns are overlaid and the maximum value at each location is used.
-- this allows you to implement a finite number of precedences between hyphenation rules
--
-- (e.g. @3foo.@ indicates that the suffix '-foo' should be hyphenated with precedence 3.)
insertPattern :: String -> Patterns -> Patterns
insertPattern s0 = go (chars s0) where
  pts = scorePattern s0
  go [] (Patterns _ m) = Patterns pts m
  go (x:xs) (Patterns n m) = Patterns n (IM.insertWith (\_ -> go xs) (fromEnum x) (mk xs) m)
  mk []     = Patterns pts IM.empty
  mk (x:xs) = Patterns [] (IM.singleton (fromEnum x) (mk xs))

-- | Parse one pattern per line from an input string
--
-- @hyph-utf8@ supplies these files UTF-8 encoded in the @txt@ folder with a @.pat.txt@ extension
parsePatterns :: String -> Patterns
parsePatterns = foldr insertPattern mempty . lines

chars :: String -> String
chars = filter (\x -> x < '0' || x > '9')

-- | Convert a Pattern string to a Score
scorePattern :: String -> [Int]
scorePattern [] = [0]
scorePattern (x:ys)
  | isDigit x = digitToInt x : if null ys then [] else scorePattern (tail ys)
  | otherwise = 0 : scorePattern ys

-- | Zip two scores.
zipMax :: [Int] -> [Int] -> [Int]
zipMax (x:xs) (y:ys) = max x y : zipMax xs ys
zipMax [] ys = ys
zipMax xs [] = xs

