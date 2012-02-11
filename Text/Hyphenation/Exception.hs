-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Exception
-- Copyright   :  (C) 2012 Edward Kmett,
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
import Data.Monoid
import Prelude hiding (lookup)

-- manually supplied hyphenations
newtype Exceptions = Exceptions (HM.HashMap String [Int])
  deriving Show

zipMin :: [Int] -> [Int] -> [Int]
zipMin (x:xs) (y:ys) = min x y : zipMin xs ys
zipMin _ _ = []

-- | Exceptions permit an exact list of hyphenation locations
-- but merging exceptions is used to restrict the set when both contain the same word
instance Monoid Exceptions where
  mempty = Exceptions mempty
  Exceptions m `mappend` Exceptions n = Exceptions (HM.unionWith zipMin m n)

-- | add an exception to the exception table.
-- if it is already present, this will restrict the set of hyphenations to the
-- intersection of the set provided and the set present.
addException :: String -> Exceptions -> Exceptions
addException s (Exceptions m) = Exceptions $
  HM.insertWith zipMin (filter (/= '-') s) (scoreException s) m

lookupException :: String -> Exceptions -> Maybe [Int]
lookupException s (Exceptions m) = HM.lookup s m

scoreException :: String -> [Int]
scoreException []         = [0]
scoreException (x:ys)
  | x == '-'  = 1 : if null ys then [] else scoreException (tail ys)
  | otherwise = 0 : scoreException ys

-- parse one exception per line from an input string
parseExceptions :: String -> Exceptions
parseExceptions = foldr addException mempty . lines
