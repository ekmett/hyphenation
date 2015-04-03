-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Hyphenator
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Hyphenation based on the Knuth-Liang algorithm as used by TeX.
----------------------------------------------------------------------------
module Text.Hyphenation.Hyphenator
  ( Hyphenator(..)
  -- * Hyphenate with a given set of patterns
  , hyphenate
  , defaultLeftMin
  , defaultRightMin
  ) where

import Text.Hyphenation.Pattern
import Text.Hyphenation.Exception

-- | By default, do not insert hyphens in the first two characters
--
-- >>> defaultLeftMin
-- 2
defaultLeftMin :: Int
defaultLeftMin = 2
-- | By default, do not insert hyphens in the last three characters.
--
-- >>> defaultRightMin
-- 3
defaultRightMin :: Int
defaultRightMin = 3

-- | A @Hyphenator@ is combination of an alphabet normalization scheme, a set of 'Patterns', a set of 'Exceptions' to those patterns
-- and a number of characters at each end to skip hyphenating.
data Hyphenator = Hyphenator
  { hyphenatorChars      :: Char -> Char        -- ^ a normalization function applied to input characters before applying patterns or exceptions
  , hyphenatorPatterns   :: Patterns            -- ^ hyphenation patterns stored in a trie
  , hyphenatorExceptions :: Exceptions          -- ^ exceptions to the general hyphenation rules, hyphenated manually
  , hyphenatorLeftMin    :: {-# UNPACK #-} !Int -- ^ the number of characters as the start of a word to skip hyphenating, by default: 2
  , hyphenatorRightMin   :: {-# UNPACK #-} !Int -- ^ the number of characters at the end of the word to skip hyphenating, by default: 3
  }

-- | Using a 'Hyphenator', compute the score of a string.
hyphenationScore :: Hyphenator -> String -> [Int]
hyphenationScore (Hyphenator nf ps es l r) s
  | l + r >= n = replicate (n + 1) 0
  | otherwise = case lookupException ls es of
    Just pts -> trim pts
    Nothing -> trim (lookupPattern ls ps)
  where
    trim result = replicate l 0 ++ take (n - l - r) (drop l result)
    n  = length s
    ls = map nf s

-- | hyphenate a single word using the specified Hyphenator. Returns a set of candidate breakpoints by decomposing the input
-- into substrings.
--
-- >>> import Text.Hyphenation
--
-- >>> hyphenate english_US "supercalifragilisticexpialadocious"
-- ["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
--
-- >>> hyphenate english_US "hyphenation"
-- ["hy","phen","ation"]
hyphenate :: Hyphenator -> String -> [String]
hyphenate h s0 = go [] s0 $ tail $ hyphenationScore h s0 where
  go acc (w:ws) (p:ps)
    | odd p     = reverse (w:acc) : go [] ws ps
    | otherwise = go (w:acc) ws ps
  go acc ws _  = [reverse acc ++ ws]
