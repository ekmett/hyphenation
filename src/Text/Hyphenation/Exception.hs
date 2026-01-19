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
  , lookupExceptionText
  , lookupExceptionUtf8
  , scoreException
  , parseExceptions
  ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Prelude hiding (lookup)

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- | Hyphenation exceptions are special cases that should use the specified hyphenation points.
data Exceptions = Exceptions
  { exceptionsString :: HM.HashMap String [Int]
  , exceptionsText   :: HM.HashMap T.Text [Int]
  , exceptionsUtf8   :: HM.HashMap BS.ByteString [Int]
  }
  deriving Show

zipMin :: [Int] -> [Int] -> [Int]
zipMin (x:xs) (y:ys) = min x y : zipMin xs ys
zipMin _ _ = []

-- | Exceptions permit an exact list of hyphenation locations
-- but merging exceptions is used to restrict the set when both contain the same word
instance Semigroup Exceptions where
  Exceptions ms mt mu <> Exceptions ns nt nu =
    Exceptions (HM.unionWith zipMin ms ns) (HM.unionWith zipMin mt nt) (HM.unionWith zipMin mu nu)

-- | Exceptions permit an exact list of hyphenation locations
-- but merging exceptions is used to restrict the set when both contain the same word
instance Monoid Exceptions where
  mempty = Exceptions mempty mempty mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

-- | add an exception to the exception table.
-- if it is already present, this will restrict the set of hyphenations to the
-- intersection of the set provided and the set present.
addException :: String -> Exceptions -> Exceptions
addException s (Exceptions ms mt mu) = Exceptions ms' mt' mu'
  where
    key = filter (/= '-') s
    score = scoreException s
    ms' = HM.insertWith zipMin key score ms
    tkey = T.pack key
    mt' = HM.insertWith zipMin tkey score mt
    mu' = HM.insertWith zipMin (TE.encodeUtf8 tkey) score mu

-- | Try to find a matching hyphenation exception.
lookupException :: String -> Exceptions -> Maybe [Int]
lookupException s = HM.lookup s . exceptionsString

-- | Try to find a matching hyphenation exception.
lookupExceptionText :: T.Text -> Exceptions -> Maybe [Int]
lookupExceptionText s = HM.lookup s . exceptionsText

-- | Try to find a matching hyphenation exception from UTF-8 bytes.
lookupExceptionUtf8 :: BS.ByteString -> Exceptions -> Maybe [Int]
lookupExceptionUtf8 s = HM.lookup s . exceptionsUtf8

-- | Convert an exception string to a score.
scoreException :: String -> [Int]
scoreException []         = [0]
scoreException (x:ys)
  | x == '-'  = 1 : case ys of
                      []    -> []
                      _:ys' -> scoreException ys'
  | otherwise = 0 : scoreException ys

-- | Parse one exception per line from an input string
parseExceptions :: String -> Exceptions
parseExceptions = foldr addException mempty . lines
