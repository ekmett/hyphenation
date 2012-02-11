-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Language
-- Copyright   :  (C) 2012 Edward Kmett,
--                (C) 2007 Ned Batchelder
-- License     :  BSD-style (see the languageAffix LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Text.Hyphenation.Language
  (
  -- * Pattern file support
    Language(..)
  , languageHyphenator
  -- * Provided language hyphenators
  , afrikaans, basque, bengali, bulgarian, catalan, chinese
  , coptic, croatian, czech, danish, dutch, english_US, english_GB, esperanto
  , estonian, ethiopic, farsi, finnish, french, galician, german_1901, german_1996
  , german_Swiss, greek_Ancient, greek_Mono, greek_Poly, gujarati, hindi, hungarian
  , icelandic, indonesian, interlingua, irish, italian, kannada, kurmanji, lao, latin
  , latvian, lithuanian, malayalam, marathi, mongolian, norwegian_Bokmal
  , norwegian_Nynorsk, oriya, panjabi, polish, portuguese, romanian
  , russian, sanskrit, serbian_Cyrillic, serbocroatian_Cyrillic
  , serbocroatian_Latin, slovak, slovenian, spanish, swedish, tamil
  , telugu, turkish, turkmen, ukrainian, uppersorbian, welsh
  , loadHyphenator
  , languageAffix
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.IntMap as IM
import Text.Hyphenation
import Text.Hyphenation.Pattern
import Text.Hyphenation.Exception
import System.IO.Unsafe
import Paths_hyphenation

chrLine :: String -> [(Int, Char)]
chrLine (x:xs) = map (\y -> (fromEnum y, x)) xs
chrLine [] = []

-- | Read a built-in language file from the data directory where cabal installed this package.
--
-- (e.g. @hyphenateLanguage \"en\"@ opens @\"\/Users\/ekmett\/.cabal\/lib\/hyphenation-0.1\/ghc-7.4.1\/en.hyp\"@
-- when run on the author's local machine)
loadHyphenator :: String -> IO Hyphenator
loadHyphenator language = do
  hyp <- getDataFileName ("hyph-" ++ language ++ ".hyp.txt") >>= readFile
  pat <- getDataFileName ("hyph-" ++ language ++ ".pat.txt") >>= readFile
  chr <- getDataFileName ("hyph-" ++ language ++ ".chr.txt") >>= readFile
  let chrMap = IM.fromList (lines chr >>= chrLine)
      tryLookup x = fromMaybe x $ IM.lookup (fromEnum x) chrMap
  return $ Hyphenator tryLookup (parsePatterns pat) (parseExceptions hyp) defaultLeftMin defaultRightMin

data Language
  = Afrikaans
  | Basque
  | Bengali
  | Bulgarian
  | Catalan
  | Chinese
  | Coptic
  | Croatian
  | Czech
  | Danish
  | Dutch
  | English_US | English_GB
  | Esperanto
  | Estonian
  | Ethiopic
  | Farsi
  | Finnish
  | French
  | Galician
  | German_1901 | German_1996 | German_Swiss
  | Greek_Ancient
  | Greek_Mono
  | Greek_Poly
  | Gujarati
  | Hindi
  | Hungarian
  | Icelandic
  | Indonesian
  | Interlingua
  | Irish
  | Italian
  | Kannada
  | Kurmanji
  | Lao
  | Latin
  | Latvian
  | Lithuanian
  | Malayalam
  | Marathi
  | Mongolian
  | Norwegian_Bokmal | Norwegian_Nynorsk
  | Oriya
  | Panjabi
  | Polish
  | Portuguese
  | Romanian
  | Russian
  | Sanskrit
  | Serbian_Cyrillic
  | Serbocroatian_Cyrillic | Serbocroatian_Latin
  | Slovak
  | Slovenian
  | Spanish
  | Swedish
  | Tamil
  | Telugu
  | Turkish
  | Turkmen
  | Ukrainian
  | Uppersorbian
  | Welsh
  deriving (Eq,Ord,Show,Bounded,Enum)


-- | the infix portion of the data file names used for this language
languageAffix :: Language -> String
languageAffix s = case s of
  Afrikaans -> "af"
  Basque -> "eu"
  Bengali -> "bn"
  Bulgarian -> "bg"
  Catalan -> "ca"
  Chinese -> "zh-latn-pinyin"
  Coptic -> "cop"
  Croatian -> "hr"
  Czech -> "cs"
  Danish -> "da"
  Dutch -> "nl"
  English_US -> "en-us"
  English_GB -> "en-gb"
  Esperanto -> "eo"
  Estonian -> "et"
  Ethiopic -> "mul-ethi"
  Farsi -> "fa"
  Finnish -> "fi"
  French -> "fr"
  Galician -> "gl"
  German_1901  -> "de-1901"
  German_1996  -> "de-1996"
  German_Swiss -> "de-ch-1901"
  Greek_Ancient -> "grc"
  Greek_Mono -> "el-monoton"
  Greek_Poly -> "el-polyton"
  Gujarati -> "gu"
  Hindi -> "hi"
  Hungarian -> "hu"
  Icelandic -> "is"
  Indonesian -> "id"
  Interlingua -> "ia"
  Irish -> "ga"
  Italian -> "it"
  Kannada -> "kn"
  Kurmanji -> "kmr"
  Lao -> "lo"
  Latin -> "la"
  Latvian -> "lv"
  Lithuanian -> "lt"
  Malayalam -> "ml"
  Marathi -> "mr"
  Mongolian -> "mn-cyrl"
  Norwegian_Bokmal  -> "nb"
  Norwegian_Nynorsk -> "nn"
  Oriya -> "or"
  Panjabi -> "pa"
  Polish -> "pl"
  Portuguese -> "pt"
  Romanian -> "ro"
  Russian -> "ru"
  Sanskrit -> "sa"
  Serbian_Cyrillic -> "sr-cyrl"
  Serbocroatian_Cyrillic -> "sh-cyrl"
  Serbocroatian_Latin -> "sh-latn"
  Slovak -> "sk"
  Slovenian -> "sl"
  Spanish -> "es"
  Swedish -> "sv"
  Tamil -> "ta"
  Telugu -> "te"
  Turkish -> "tr"
  Turkmen -> "tk"
  Ukrainian -> "uk"
  Uppersorbian -> "hsb"
  Welsh -> "cy"


-- |
-- > ghci> hyphenate english_US "supercalifragilisticexpialadocious"
-- > ["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
--
-- favors US hyphenation
english_US :: Hyphenator

-- |
-- > ghci> hyphenate english_GB "supercalifragilisticexpialadocious"
-- > ["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
--
-- favors UK hyphenation
english_GB :: Hyphenator

-- |
-- > ghci> hyphenate french "anticonstitutionnellement"
-- > ["an","ti","cons","ti","tu","tion","nel","le","ment"]
french :: Hyphenator

-- |
-- > ghci> hyphenate icelandic "vaðlaheiðavegavinnuverkfærageymsluskúr"
-- > ["va\240la","hei\240a","vega","vinnu","verk","f\230ra","geymslu","sk\250r"]
icelandic :: Hyphenator


afrikaans, basque, bengali, bulgarian, catalan, chinese,
 coptic, croatian, czech, danish, dutch, esperanto,
 estonian, ethiopic, farsi, finnish, galician, german_1901, german_1996,
 german_Swiss, greek_Ancient, greek_Mono, greek_Poly, gujarati, hindi, hungarian,
 indonesian, interlingua, irish, italian, kannada, kurmanji, lao, latin,
 latvian, lithuanian, malayalam, marathi, mongolian, norwegian_Bokmal,
 norwegian_Nynorsk, oriya, panjabi, polish, portuguese, romanian,
 russian, sanskrit, serbian_Cyrillic, serbocroatian_Cyrillic,
 serbocroatian_Latin, slovak, slovenian, spanish, swedish, tamil,
 telugu, turkish, turkmen, ukrainian, uppersorbian, welsh :: Hyphenator

afrikaans = unsafePerformIO (loadHyphenator (languageAffix Afrikaans))
basque = unsafePerformIO (loadHyphenator (languageAffix Basque))
bengali = unsafePerformIO (loadHyphenator (languageAffix Bengali))
bulgarian = unsafePerformIO (loadHyphenator (languageAffix Bulgarian))
catalan = unsafePerformIO (loadHyphenator (languageAffix Catalan))
chinese = unsafePerformIO (loadHyphenator (languageAffix Chinese))
coptic = unsafePerformIO (loadHyphenator (languageAffix Coptic))
croatian = unsafePerformIO (loadHyphenator (languageAffix Croatian))
czech = unsafePerformIO (loadHyphenator (languageAffix Czech))
danish = unsafePerformIO (loadHyphenator (languageAffix Danish))
dutch = unsafePerformIO (loadHyphenator (languageAffix Dutch))
english_US = unsafePerformIO (loadHyphenator (languageAffix English_US))
english_GB = unsafePerformIO (loadHyphenator (languageAffix English_GB))
esperanto = unsafePerformIO (loadHyphenator (languageAffix Esperanto))
estonian = unsafePerformIO (loadHyphenator (languageAffix Estonian))
ethiopic = unsafePerformIO (loadHyphenator (languageAffix Ethiopic))
farsi = unsafePerformIO (loadHyphenator (languageAffix Farsi))
finnish = unsafePerformIO (loadHyphenator (languageAffix Finnish))
french = unsafePerformIO (loadHyphenator (languageAffix French))
galician = unsafePerformIO (loadHyphenator (languageAffix Galician))
german_1901 = unsafePerformIO (loadHyphenator (languageAffix German_1901))
german_1996 = unsafePerformIO (loadHyphenator (languageAffix German_1996))
german_Swiss = unsafePerformIO (loadHyphenator (languageAffix German_Swiss))
greek_Ancient = unsafePerformIO (loadHyphenator (languageAffix Greek_Ancient))
greek_Mono = unsafePerformIO (loadHyphenator (languageAffix Greek_Mono))
greek_Poly = unsafePerformIO (loadHyphenator (languageAffix Greek_Poly))
gujarati = unsafePerformIO (loadHyphenator (languageAffix Gujarati))
hindi = unsafePerformIO (loadHyphenator (languageAffix Hindi))
hungarian = unsafePerformIO (loadHyphenator (languageAffix Hungarian))
icelandic = unsafePerformIO (loadHyphenator (languageAffix Icelandic))
indonesian = unsafePerformIO (loadHyphenator (languageAffix Indonesian))
interlingua = unsafePerformIO (loadHyphenator (languageAffix Interlingua))
irish = unsafePerformIO (loadHyphenator (languageAffix Irish))
italian = unsafePerformIO (loadHyphenator (languageAffix Italian))
kannada = unsafePerformIO (loadHyphenator (languageAffix Kannada))
kurmanji = unsafePerformIO (loadHyphenator (languageAffix Kurmanji))
lao = unsafePerformIO (loadHyphenator (languageAffix Lao))
latin = unsafePerformIO (loadHyphenator (languageAffix Latin))
latvian = unsafePerformIO (loadHyphenator (languageAffix Latvian))
lithuanian = unsafePerformIO (loadHyphenator (languageAffix Lithuanian))
malayalam = unsafePerformIO (loadHyphenator (languageAffix Malayalam))
marathi = unsafePerformIO (loadHyphenator (languageAffix Marathi))
mongolian = unsafePerformIO (loadHyphenator (languageAffix Mongolian))
norwegian_Bokmal = unsafePerformIO (loadHyphenator (languageAffix Norwegian_Bokmal))
norwegian_Nynorsk = unsafePerformIO (loadHyphenator (languageAffix Norwegian_Nynorsk))
oriya = unsafePerformIO (loadHyphenator (languageAffix Oriya))
panjabi = unsafePerformIO (loadHyphenator (languageAffix Panjabi))
polish = unsafePerformIO (loadHyphenator (languageAffix Polish))
portuguese = unsafePerformIO (loadHyphenator (languageAffix Portuguese))
romanian = unsafePerformIO (loadHyphenator (languageAffix Romanian))
russian = unsafePerformIO (loadHyphenator (languageAffix Russian))
sanskrit = unsafePerformIO (loadHyphenator (languageAffix Sanskrit))
serbian_Cyrillic = unsafePerformIO (loadHyphenator (languageAffix Serbian_Cyrillic))
serbocroatian_Cyrillic = unsafePerformIO (loadHyphenator (languageAffix Serbocroatian_Cyrillic))
serbocroatian_Latin = unsafePerformIO (loadHyphenator (languageAffix Serbocroatian_Latin))
slovak = unsafePerformIO (loadHyphenator (languageAffix Slovak))
slovenian = unsafePerformIO (loadHyphenator (languageAffix Slovenian))
spanish = unsafePerformIO (loadHyphenator (languageAffix Spanish))
swedish = unsafePerformIO (loadHyphenator (languageAffix Swedish))
tamil = unsafePerformIO (loadHyphenator (languageAffix Tamil))
telugu = unsafePerformIO (loadHyphenator (languageAffix Telugu))
turkish = unsafePerformIO (loadHyphenator (languageAffix Turkish))
turkmen = unsafePerformIO (loadHyphenator (languageAffix Turkmen))
ukrainian = unsafePerformIO (loadHyphenator (languageAffix Ukrainian))
uppersorbian = unsafePerformIO (loadHyphenator (languageAffix Uppersorbian))
welsh = unsafePerformIO (loadHyphenator (languageAffix Welsh))

languageHyphenator :: Language -> Hyphenator
languageHyphenator s = case s of
  Afrikaans -> afrikaans
  Basque -> basque
  Bengali -> bengali
  Bulgarian -> bulgarian
  Catalan -> catalan
  Chinese -> chinese
  Coptic -> coptic
  Croatian -> croatian
  Czech -> czech
  Danish -> danish
  Dutch -> dutch
  English_US -> english_US
  English_GB -> english_GB
  Esperanto -> esperanto
  Estonian -> estonian
  Ethiopic -> ethiopic
  Farsi -> farsi
  Finnish -> finnish
  French -> french
  Galician -> galician
  German_1901  -> german_1901
  German_1996  -> german_1996
  German_Swiss -> german_Swiss
  Greek_Ancient -> greek_Ancient
  Greek_Mono -> greek_Mono
  Greek_Poly -> greek_Poly
  Gujarati -> gujarati
  Hindi -> hindi
  Hungarian -> hungarian
  Icelandic -> icelandic
  Indonesian -> indonesian
  Interlingua -> interlingua
  Irish -> irish
  Italian -> italian
  Kannada -> kannada
  Kurmanji -> kurmanji
  Lao -> lao
  Latin -> latin
  Latvian -> latvian
  Lithuanian -> lithuanian
  Malayalam -> malayalam
  Marathi -> marathi
  Mongolian -> mongolian
  Norwegian_Bokmal  -> norwegian_Bokmal
  Norwegian_Nynorsk -> norwegian_Nynorsk
  Oriya -> oriya
  Panjabi -> panjabi
  Polish -> polish
  Portuguese -> portuguese
  Romanian -> romanian
  Russian -> russian
  Sanskrit -> sanskrit
  Serbian_Cyrillic -> serbian_Cyrillic
  Serbocroatian_Cyrillic -> serbocroatian_Cyrillic
  Serbocroatian_Latin -> serbocroatian_Latin
  Slovak -> slovak
  Slovenian -> slovenian
  Spanish -> spanish
  Swedish -> swedish
  Tamil -> tamil
  Telugu -> telugu
  Turkish -> turkish
  Turkmen -> turkmen
  Ukrainian -> ukrainian
  Uppersorbian -> uppersorbian
  Welsh -> welsh

