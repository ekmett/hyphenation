-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Hyphenator
-- Copyright   :  (C) 2012-2019 Edward Kmett
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
  , hyphenateText
  , hyphenateUtf8
  , hyphenateShortText
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as TS
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import Data.Char (chr, ord)
import Text.Hyphenation.Pattern
import Text.Hyphenation.Exception

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
  | l + r > n = replicate (n + 1) 0
  | otherwise = case lookupException ls es of
    Just pts -> trim pts
    Nothing -> trim (lookupPattern ls ps)
  where
    trim pts = replicate l 0 ++ take (n - l - r + 1) (drop l pts)
    n  = length s
    ls = map nf s

-- | Using a 'Hyphenator', compute the score of a 'T.Text'.
hyphenationScoreText :: Hyphenator -> T.Text -> [Int]
hyphenationScoreText (Hyphenator nf ps es l r) s =
  if l + r > n
    then replicate m 0
    else replicate l 0 ++ take (m - l - r) (drop l result)
  where
    ls = T.map nf s
    result = case lookupExceptionText ls es of
      Just pts -> pts
      Nothing -> lookupPatternText ls ps
    m = length result
    n = m - 1

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
hyphenate h s0 = go [] s0 score' where
  score' =
    case hyphenationScore h s0 of
      _:ps -> ps
      []   -> error $ "Malformed hyphenation score for " ++ s0

  go acc (w:ws) (p:ps)
    | odd p     = reverse (w:acc) : go [] ws ps
    | otherwise = go (w:acc) ws ps
  go acc ws _  = [reverse acc ++ ws]

-- | Hyphenate a 'T.Text' value, returning hyphenated segments.
hyphenateText :: Hyphenator -> T.Text -> [T.Text]
hyphenateText h s0 =
  case hyphenationScoreText h s0 of
    _:ps -> splitText s0 ps
    []   -> error $ "Malformed hyphenation score for " ++ T.unpack s0
  where
    splitText t scores = case t of
      TI.Text arr off len -> go 0 0 scores []
        where
          go i segStart ps acc
            | i >= len = reverse (TI.Text arr (off + segStart) (i - segStart) : acc)
            | otherwise = case ps of
                [] -> reverse (TI.Text arr (off + segStart) (len - segStart) : acc)
                p:ps' ->
                  let TU.Iter _ step = TU.iter t i
                      i' = i + step
                      (acc', segStart') =
                        if odd p
                          then (TI.Text arr (off + segStart) (i' - segStart) : acc, i')
                          else (acc, segStart)
                  in go i' segStart' ps' acc'

-- | Hyphenate a UTF-8 'BS.ByteString', returning UTF-8 segments.
hyphenateUtf8 :: Hyphenator -> BS.ByteString -> [BS.ByteString]
hyphenateUtf8 h bs =
  case normalizeAscii h bs of
    Just norm -> hyphenateAscii h bs norm
    Nothing ->
      fmap TE.encodeUtf8 . hyphenateText h . TE.decodeUtf8With lenientDecode $ bs

hyphenateAscii :: Hyphenator -> BS.ByteString -> BS.ByteString -> [BS.ByteString]
hyphenateAscii h bs norm =
  case hyphenationScoreAscii h norm of
    _:ps -> splitAscii bs ps
    []   -> error $ "Malformed hyphenation score for ASCII input"

hyphenationScoreAscii :: Hyphenator -> BS.ByteString -> [Int]
hyphenationScoreAscii (Hyphenator _ ps es l r) norm =
  if l + r > n
    then replicate (n + 1) 0
    else trim result
  where
    n = BS.length norm
    trim pts = replicate l 0 ++ take (n - l - r + 1) (drop l pts)
    result = case lookupExceptionUtf8 norm es of
      Just pts -> pts
      Nothing -> lookupPatternAscii norm ps

splitAscii :: BS.ByteString -> [Int] -> [BS.ByteString]
splitAscii bs scores = go 0 0 scores []
  where
    len = BS.length bs
    go i segStart ps acc
      | i >= len = reverse (slice segStart (i - segStart) : acc)
      | otherwise = case ps of
          [] -> reverse (BS.drop segStart bs : acc)
          p:ps' ->
            let i' = i + 1
                (acc', segStart') =
                  if odd p
                    then (slice segStart (i' - segStart) : acc, i')
                    else (acc, segStart)
            in go i' segStart' ps' acc'
    slice start count = BS.take count (BS.drop start bs)

normalizeAscii :: Hyphenator -> BS.ByteString -> Maybe BS.ByteString
normalizeAscii (Hyphenator nf _ _ _ _) bs
  | BS.all isAsciiNorm bs = Just (BS.map normByte bs)
  | otherwise = Nothing
  where
    isAsciiNorm w = ord (nf (chr (fromIntegral w))) < 0x80
    normByte w = fromIntegral (ord (nf (chr (fromIntegral w))))

-- | Hyphenate a 'TS.ShortText' value, returning hyphenated segments.
hyphenateShortText :: Hyphenator -> TS.ShortText -> [TS.ShortText]
hyphenateShortText h = fmap TS.fromText . hyphenateText h . TS.toText
