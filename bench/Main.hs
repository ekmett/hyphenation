module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as TS
import Criterion.Main
import Text.Hyphenation

main :: IO ()
main = do
  let wordString = "supercalifragilisticexpialadocious"
      wordText = T.pack wordString
      wordUtf8 = TE.encodeUtf8 wordText
      wordShortText = TS.pack wordString
      longWordString = concat (replicate 200 wordString)
      longWordText = T.pack longWordString
      longWordUtf8 = TE.encodeUtf8 longWordText
      longWordShortText = TS.pack longWordString
  defaultMain
    [ bench "hyphenate String" $ nf (hyphenate english_US) wordString
    , bench "hyphenate Text" $ nf (hyphenateText english_US) wordText
    , bench "hyphenate UTF-8 ByteString" $ nf (hyphenateUtf8 english_US) wordUtf8
    , bench "hyphenate ShortText" $ nf (hyphenateShortText english_US) wordShortText
    , bench "hyphenate String (long)" $ nf (hyphenate english_US) longWordString
    , bench "hyphenate Text (long)" $ nf (hyphenateText english_US) longWordText
    , bench "hyphenate UTF-8 ByteString (long)" $ nf (hyphenateUtf8 english_US) longWordUtf8
    , bench "hyphenate ShortText (long)" $ nf (hyphenateShortText english_US) longWordShortText
    ]
