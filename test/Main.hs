module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as TS
import Test.HUnit
import Text.Hyphenation

main :: IO ()
main = do
  _ <- runTestTT tests
  pure ()

tests :: Test
tests = TestList
  [ "english_US hyphenation" ~: hyphenate english_US "hyphenation"
      ~?= ["hy","phen","ation"]
  , "english_US supercalifragilisticexpialadocious" ~:
      hyphenate english_US "supercalifragilisticexpialadocious"
      ~?= ["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
  , "languageHyphenator English_US" ~:
      hyphenate (languageHyphenator English_US) "hyphenation"
      ~?= ["hy","phen","ation"]
  , "left/right min prevents breaks" ~:
      hyphenate english_US "word"
      ~?= ["word"]
  , "hyphenateText" ~:
      hyphenateText english_US (T.pack "hyphenation")
      ~?= fmap T.pack ["hy","phen","ation"]
  , "hyphenateUtf8" ~:
      hyphenateUtf8 english_US (TE.encodeUtf8 (T.pack "hyphenation"))
      ~?= fmap TE.encodeUtf8 (fmap T.pack ["hy","phen","ation"])
  , "hyphenateShortText" ~:
      hyphenateShortText english_US (TS.pack "hyphenation")
      ~?= fmap TS.pack ["hy","phen","ation"]
  ]
