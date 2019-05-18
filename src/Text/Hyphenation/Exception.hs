{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Exception
-- Copyright   :  (C) 2012-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Text.Hyphenation.Exception
  (
  -- * Pattern file support
    Exceptions
  , addException
  , lookupException
  , scoreException
  , parseExceptions
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Prelude hiding (lookup)

-- | Hyphenation exceptions are special cases that should use the specified hyphenation points.
newtype Exceptions = Exceptions (HM.HashMap String [Int])
  deriving Show

zipMin :: [Int] -> [Int] -> [Int]
zipMin (x:xs) (y:ys) = min x y : zipMin xs ys
zipMin _ _ = []

-- | Exceptions permit an exact list of hyphenation locations
-- but merging exceptions is used to restrict the set when both contain the same word
instance Semigroup Exceptions where
  Exceptions m <> Exceptions n = Exceptions (HM.unionWith zipMin m n)

-- | Exceptions permit an exact list of hyphenation locations
-- but merging exceptions is used to restrict the set when both contain the same word
instance Monoid Exceptions where
  mempty = Exceptions mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

-- | add an exception to the exception table.
-- if it is already present, this will restrict the set of hyphenations to the
-- intersection of the set provided and the set present.
addException :: String -> Exceptions -> Exceptions
addException s (Exceptions m) = Exceptions $
  HM.insertWith zipMin (filter (/= '-') s) (scoreException s) m

-- | Try to find a matching hyphenation exception.
lookupException :: String -> Exceptions -> Maybe [Int]
lookupException s (Exceptions m) = HM.lookup s m

-- | Convert an exception string to a score.
scoreException :: String -> [Int]
scoreException []         = [0]
scoreException (x:ys)
  | x == '-'  = 1 : if null ys then [] else scoreException (tail ys)
  | otherwise = 0 : scoreException ys

-- | Parse one exception per line from an input string
parseExceptions :: String -> Exceptions
parseExceptions = foldr addException mempty . lines
