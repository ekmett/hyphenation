module Main (main) where

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
  ]
