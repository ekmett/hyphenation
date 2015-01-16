{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Language
-- Copyright   :  (C) 2012-2013 Edward Kmett,
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
  , estonian, ethiopic, {- farsi, -} finnish, french, friulan, galician, german_1901, german_1996
  , german_Swiss, greek_Ancient, greek_Mono, greek_Poly, gujarati, hindi, hungarian
  , icelandic, indonesian, interlingua, irish, italian, kannada, kurmanji, lao, latin
  , latvian, lithuanian, malayalam, marathi, mongolian, norwegian_Bokmal
  , norwegian_Nynorsk, oriya, panjabi, piedmontese, polish, portuguese, romanian, romansh
  , russian, sanskrit, serbian_Cyrillic, serbocroatian_Cyrillic
  , serbocroatian_Latin, slovak, slovenian, spanish, swedish, tamil
  , telugu, thai, turkish, turkmen, ukrainian, uppersorbian, welsh
  , loadHyphenator
  , languageAffix
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.IntMap as IM
import Text.Hyphenation.Hyphenator
import Text.Hyphenation.Pattern
import Text.Hyphenation.Exception
import Data.FileEmbed
import Control.Arrow (second)
import Data.ByteString.Char8 (unpack)

chrLine :: String -> [(Int, Char)]
chrLine (x:xs) = map (\y -> (fromEnum y, x)) xs
chrLine [] = []

hyphenatorFiles :: [(FilePath, String)]
hyphenatorFiles = map (second unpack) $(embedDir "data")

-- | Read a built-in language file from the data directory where cabal installed this package.
--
-- (e.g. @hyphenateLanguage \"en-us\"@ opens @\"\/Users\/ekmett\/.cabal\/share\/hyphenation-0.2\/ghc-7.4.1\/hyph-en-us.hyp.txt\"@
-- among others when run on the author's local machine)
loadHyphenator :: String -> Hyphenator
loadHyphenator language = Hyphenator tryLookup (parsePatterns pat) (parseExceptions hyp) defaultLeftMin defaultRightMin
  where Just hyp = lookup ("hyph-" ++ language ++ ".hyp.txt") hyphenatorFiles
        Just pat = lookup ("hyph-" ++ language ++ ".pat.txt") hyphenatorFiles
        Just chr = lookup ("hyph-" ++ language ++ ".chr.txt") hyphenatorFiles
        chrMap = IM.fromList (lines chr >>= chrLine)
        tryLookup x = fromMaybe x $ IM.lookup (fromEnum x) chrMap

-- | A strongly typed set of available languages you can use for hyphenation.
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
  -- | Farsi
  | Finnish
  | French
  | Friulan
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
  | Piedmontese
  | Polish
  | Portuguese
  | Romanian
  | Romansh
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
  | Thai
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
  -- Farsi -> "fa"
  Finnish -> "fi"
  French -> "fr"
  Friulan -> "fur"
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
  Piedmontese -> "pms"
  Polish -> "pl"
  Portuguese -> "pt"
  Romanian -> "ro"
  Romansh -> "rm"
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
  Thai -> "th"
  Turkish -> "tr"
  Turkmen -> "tk"
  Ukrainian -> "uk"
  Uppersorbian -> "hsb"
  Welsh -> "cy"


-- |
-- >>> hyphenate english_US "supercalifragilisticexpialadocious"
-- ["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
--
-- favors US hyphenation
english_US :: Hyphenator

-- |
-- >>> hyphenate english_GB "supercalifragilisticexpialadocious"
-- ["su","per","cal","i","fra","gil","istic","ex","pi","alado","cious"]
--
-- favors UK hyphenation
english_GB :: Hyphenator

-- |
-- >>> hyphenate french "anticonstitutionnellement"
-- ["an","ti","cons","ti","tu","tion","nel","le","ment"]
french :: Hyphenator

-- |
-- >>> hyphenate icelandic "va\240lahei\240avegavinnuverkf\230rageymslusk\250r"
-- ["va\240la","hei\240a","vega","vinnu","verk","f\230ra","geymslu","sk\250r"]
icelandic :: Hyphenator

-- | Hyphenators for a wide array of languages.
afrikaans, basque, bengali, bulgarian, catalan, chinese,
 coptic, croatian, czech, danish, dutch, esperanto,
 estonian, ethiopic, {- farsi, -} finnish, friulan, galician, german_1901, german_1996,
 german_Swiss, greek_Ancient, greek_Mono, greek_Poly, gujarati, hindi, hungarian,
 indonesian, interlingua, irish, italian, kannada, kurmanji, lao, latin,
 latvian, lithuanian, malayalam, marathi, mongolian, norwegian_Bokmal,
 norwegian_Nynorsk, oriya, panjabi, piedmontese, polish, portuguese, romanian,
 romansh, russian, sanskrit, serbian_Cyrillic, serbocroatian_Cyrillic,
 serbocroatian_Latin, slovak, slovenian, spanish, swedish, tamil,
 telugu, thai, turkish, turkmen, ukrainian, uppersorbian, welsh :: Hyphenator

afrikaans = loadHyphenator (languageAffix Afrikaans)
basque = loadHyphenator (languageAffix Basque)
bengali = loadHyphenator (languageAffix Bengali)
bulgarian = loadHyphenator (languageAffix Bulgarian)
catalan = loadHyphenator (languageAffix Catalan)
chinese = loadHyphenator (languageAffix Chinese)
coptic = loadHyphenator (languageAffix Coptic)
croatian = loadHyphenator (languageAffix Croatian)
czech = loadHyphenator (languageAffix Czech)
danish = loadHyphenator (languageAffix Danish)
dutch = loadHyphenator (languageAffix Dutch)
english_US = loadHyphenator (languageAffix English_US)
english_GB = loadHyphenator (languageAffix English_GB)
esperanto = loadHyphenator (languageAffix Esperanto)
estonian = loadHyphenator (languageAffix Estonian)
ethiopic = loadHyphenator (languageAffix Ethiopic)
-- farsi = loadHyphenator (languageAffix Farsi)
finnish = loadHyphenator (languageAffix Finnish)
french = loadHyphenator (languageAffix French)
friulan = loadHyphenator (languageAffix Friulan)
galician = loadHyphenator (languageAffix Galician)
german_1901 = loadHyphenator (languageAffix German_1901)
german_1996 = loadHyphenator (languageAffix German_1996)
german_Swiss = loadHyphenator (languageAffix German_Swiss)
greek_Ancient = loadHyphenator (languageAffix Greek_Ancient)
greek_Mono = loadHyphenator (languageAffix Greek_Mono)
greek_Poly = loadHyphenator (languageAffix Greek_Poly)
gujarati = loadHyphenator (languageAffix Gujarati)
hindi = loadHyphenator (languageAffix Hindi)
hungarian = loadHyphenator (languageAffix Hungarian)
icelandic = loadHyphenator (languageAffix Icelandic)
indonesian = loadHyphenator (languageAffix Indonesian)
interlingua = loadHyphenator (languageAffix Interlingua)
irish = loadHyphenator (languageAffix Irish)
italian = loadHyphenator (languageAffix Italian)
kannada = loadHyphenator (languageAffix Kannada)
kurmanji = loadHyphenator (languageAffix Kurmanji)
lao = loadHyphenator (languageAffix Lao)
latin = loadHyphenator (languageAffix Latin)
latvian = loadHyphenator (languageAffix Latvian)
lithuanian = loadHyphenator (languageAffix Lithuanian)
malayalam = loadHyphenator (languageAffix Malayalam)
marathi = loadHyphenator (languageAffix Marathi)
mongolian = loadHyphenator (languageAffix Mongolian)
norwegian_Bokmal = loadHyphenator (languageAffix Norwegian_Bokmal)
norwegian_Nynorsk = loadHyphenator (languageAffix Norwegian_Nynorsk)
oriya = loadHyphenator (languageAffix Oriya)
panjabi = loadHyphenator (languageAffix Panjabi)
piedmontese = loadHyphenator (languageAffix Piedmontese)
polish = loadHyphenator (languageAffix Polish)
portuguese = loadHyphenator (languageAffix Portuguese)
romanian = loadHyphenator (languageAffix Romanian)
romansh = loadHyphenator (languageAffix Romansh)
russian = loadHyphenator (languageAffix Russian)
sanskrit = loadHyphenator (languageAffix Sanskrit)
serbian_Cyrillic = loadHyphenator (languageAffix Serbian_Cyrillic)
serbocroatian_Cyrillic = loadHyphenator (languageAffix Serbocroatian_Cyrillic)
serbocroatian_Latin = loadHyphenator (languageAffix Serbocroatian_Latin)
slovak = loadHyphenator (languageAffix Slovak)
slovenian = loadHyphenator (languageAffix Slovenian)
spanish = loadHyphenator (languageAffix Spanish)
swedish = loadHyphenator (languageAffix Swedish)
tamil = loadHyphenator (languageAffix Tamil)
telugu = loadHyphenator (languageAffix Telugu)
thai = loadHyphenator (languageAffix Thai)
turkish = loadHyphenator (languageAffix Turkish)
turkmen = loadHyphenator (languageAffix Turkmen)
ukrainian = loadHyphenator (languageAffix Ukrainian)
uppersorbian = loadHyphenator (languageAffix Uppersorbian)
welsh = loadHyphenator (languageAffix Welsh)

-- | Load (and cache) the hyphenator for a given language.
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
  -- Farsi -> farsi
  Finnish -> finnish
  French -> french
  Friulan -> friulan
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
  Piedmontese -> piedmontese
  Polish -> polish
  Portuguese -> portuguese
  Romanian -> romanian
  Romansh -> romansh
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
  Thai -> thai
  Turkish -> turkish
  Turkmen -> turkmen
  Ukrainian -> ukrainian
  Uppersorbian -> uppersorbian
  Welsh -> welsh
