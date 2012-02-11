-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Hyphenator
-- Copyright   :  (C) 2012 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Hyphenation based on the Knuth-Liang algorithm as used by TeX.
--
-- The implementation is based on Ned Batchelder's public domain @hyphenate.py@
-- and simplified to remove the need for a manual exception list.
----------------------------------------------------------------------------
module Text.Hyphenation.Hyphenator
  ( Hyphenator(..)
  , hyphenationScore
  -- * Hyphenate with a given set of patterns
  , hyphenate
  , defaultLeftMin
  , defaultRightMin
  ) where

import Text.Hyphenation.Pattern
import Text.Hyphenation.Exception

defaultLeftMin, defaultRightMin :: Int
defaultLeftMin = 2
defaultRightMin = 3

data Hyphenator = Hyphenator
  { hyphenatorChars      :: Char -> Char
  , hyphenatorPatterns   :: Patterns
  , hyphenatorExceptions :: Exceptions
  , hyphenatorLeftMin    :: {-# UNPACK #-} !Int
  , hyphenatorRightMin   :: {-# UNPACK #-} !Int
  }

hyphenationScore :: Hyphenator -> String -> [Int]
hyphenationScore (Hyphenator nf ps es l r) s
  | l + r >= n = replicate (n + 1) 0
  | otherwise = case lookupException ls es of
    Just pts -> trim pts
    Nothing -> trim (lookupPattern ls ps)
  where
    trim result = replicate l 0 ++ take (n - l - r) (drop l result) ++ replicate r 0 -- drop the final replicate?
    n  = length s
    ls = map nf s

hyphenate :: Hyphenator -> String -> [String]
hyphenate h s0 = go [] s0 $ tail $ hyphenationScore h s0 where
  go acc (w:ws) (p:ps)
    | odd p     = reverse (w:acc) : go [] ws ps
    | otherwise = go (w:acc) ws ps
  go acc [] _ = [reverse acc]
  go acc ws [] = [reverse acc ++ ws]
