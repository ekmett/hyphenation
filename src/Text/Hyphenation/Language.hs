{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if EMBED
{-# LANGUAGE TemplateHaskell #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.Language
-- Copyright   :  (C) 2012-2015 Edward Kmett,
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
  , afrikaans, armenian, assamese, basque, bengali, bulgarian, catalan, chinese
  , coptic, croatian, czech, danish, dutch, english_US, english_GB, esperanto
  , estonian, ethiopic, {- farsi, -} finnish, french, friulan, galician, georgian, german_1901, german_1996
  , german_Swiss, greek_Ancient, greek_Mono, greek_Poly, gujarati, hindi, hungarian
  , icelandic, indonesian, interlingua, irish, italian, kannada, kurmanji, latin, latin_Classic
  , latvian, lithuanian, malayalam, marathi, mongolian, norwegian_Bokmal
  , norwegian_Nynorsk, occitan, oriya, panjabi, piedmontese, polish, portuguese, romanian, romansh
  , russian, sanskrit, serbian_Cyrillic, serbocroatian_Cyrillic
  , serbocroatian_Latin, slovak, slovenian, spanish, swedish, tamil
  , telugu, thai, turkish, turkmen, ukrainian, uppersorbian, welsh
  , loadHyphenator
  , languageAffix
  ) where

import Codec.Compression.GZip
#if __GLASGOW_HASKELL__ < 710
import Data.Functor ((<$>))
#endif
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Hyphenation.ByteStringLazyCompat as Lazy
import Text.Hyphenation.Hyphenator
import Text.Hyphenation.Pattern
import Text.Hyphenation.Exception
import System.IO.Unsafe

#if !EMBED
import Paths_hyphenation
#else
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Strict

hyphenatorFiles :: [(FilePath, Strict.ByteString)]
hyphenatorFiles = $(embedDir "data")
#endif

chrLine :: String -> [(Int, Char)]
chrLine (x:xs) = fmap (\y -> (fromEnum y, x)) xs
chrLine [] = []

-- | Read a built-in language file from the data directory where cabal installed this package.
--
-- (e.g. @hyphenateLanguage \"en-us\"@ opens @\"\/Users\/ekmett\/.cabal\/share\/hyphenation-0.2\/ghc-7.4.1\/hyph-en-us.hyp.txt\"@
-- among others when run on the author's local machine)
loadHyphenator :: String -> IO Hyphenator
#if !EMBED
loadHyphenator language = do
  hyp <- unzipUtf8 <$> (getDataFileName ("hyph-" ++ language ++ ".hyp.txt.gz") >>= Lazy.readFile)
  pat <- unzipUtf8 <$> (getDataFileName ("hyph-" ++ language ++ ".pat.txt.gz") >>= Lazy.readFile)
  chr <- unzipUtf8 <$> (getDataFileName ("hyph-" ++ language ++ ".chr.txt.gz") >>= Lazy.readFile)
  let chrMap = IM.fromList (Prelude.lines chr >>= chrLine)
      tryLookup x = IM.findWithDefault x (fromEnum x) chrMap
  return $ Hyphenator tryLookup (parsePatterns pat) (parseExceptions hyp) defaultLeftMin defaultRightMin
#else
loadHyphenator language = return $ Hyphenator tryLookup (parsePatterns pat) (parseExceptions hyp) defaultLeftMin defaultRightMin
  where Just hyp = unzipUtf8 . Lazy.fromStrict <$> lookup ("hyph-" ++ language ++ ".hyp.txt.gz") hyphenatorFiles
        Just pat = unzipUtf8 . Lazy.fromStrict <$> lookup ("hyph-" ++ language ++ ".pat.txt.gz") hyphenatorFiles
        Just chr = unzipUtf8 . Lazy.fromStrict <$> lookup ("hyph-" ++ language ++ ".chr.txt.gz") hyphenatorFiles
        chrMap = IM.fromList (Prelude.lines chr >>= chrLine)
        tryLookup x = IM.findWithDefault x (fromEnum x) chrMap
#endif

unzipUtf8 =
  T.unpack . T.decodeUtf8With (\ _ -> fmap (toEnum . fromEnum))
  . Lazy.toStrict . decompress

-- | A strongly typed set of available languages you can use for hyphenation.
data Language
  = Afrikaans
  | Armenian
  | Assamese
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
  | Georgian
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
  | Latin
  | Latin_Classic
  | Latvian
  | Lithuanian
  | Malayalam
  | Marathi
  | Mongolian
  | Norwegian_Bokmal | Norwegian_Nynorsk
  | Occitan
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
  Armenian -> "hy"
  Assamese -> "as"
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
  Georgian -> "ka"
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
  Latin -> "la"
  Latin_Classic -> "la-x-classic"
  Latvian -> "lv"
  Lithuanian -> "lt"
  Malayalam -> "ml"
  Marathi -> "mr"
  Mongolian -> "mn-cyrl"
  Norwegian_Bokmal  -> "nb"
  Norwegian_Nynorsk -> "nn"
  Occitan -> "oc"
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
afrikaans, armenian, assamese, basque, bengali, bulgarian, catalan, chinese,
 coptic, croatian, czech, danish, dutch, esperanto,
 estonian, ethiopic, {- farsi, -} finnish, friulan, galician, georgian, german_1901, german_1996,
 german_Swiss, greek_Ancient, greek_Mono, greek_Poly, gujarati, hindi, hungarian,
 indonesian, interlingua, irish, italian, kannada, kurmanji, latin, latin_Classic,
 latvian, lithuanian, malayalam, marathi, mongolian, norwegian_Bokmal,
 norwegian_Nynorsk, occitan, oriya, panjabi, piedmontese, polish, portuguese, romanian,
 romansh, russian, sanskrit, serbian_Cyrillic, serbocroatian_Cyrillic,
 serbocroatian_Latin, slovak, slovenian, spanish, swedish, tamil,
 telugu, thai, turkish, turkmen, ukrainian, uppersorbian, welsh :: Hyphenator

afrikaans = unsafePerformIO (loadHyphenator (languageAffix Afrikaans))
armenian = unsafePerformIO (loadHyphenator (languageAffix Armenian))
assamese = unsafePerformIO (loadHyphenator (languageAffix Assamese))
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
-- farsi = unsafePerformIO (loadHyphenator (languageAffix Farsi))
finnish = unsafePerformIO (loadHyphenator (languageAffix Finnish))
french = unsafePerformIO (loadHyphenator (languageAffix French))
friulan = unsafePerformIO (loadHyphenator (languageAffix Friulan))
galician = unsafePerformIO (loadHyphenator (languageAffix Galician))
georgian = unsafePerformIO (loadHyphenator (languageAffix Georgian))
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
latin = unsafePerformIO (loadHyphenator (languageAffix Latin))
latin_Classic = unsafePerformIO (loadHyphenator (languageAffix Latin_Classic))
latvian = unsafePerformIO (loadHyphenator (languageAffix Latvian))
lithuanian = unsafePerformIO (loadHyphenator (languageAffix Lithuanian))
malayalam = unsafePerformIO (loadHyphenator (languageAffix Malayalam))
marathi = unsafePerformIO (loadHyphenator (languageAffix Marathi))
mongolian = unsafePerformIO (loadHyphenator (languageAffix Mongolian))
norwegian_Bokmal = unsafePerformIO (loadHyphenator (languageAffix Norwegian_Bokmal))
norwegian_Nynorsk = unsafePerformIO (loadHyphenator (languageAffix Norwegian_Nynorsk))
occitan = unsafePerformIO (loadHyphenator (languageAffix Occitan))
oriya = unsafePerformIO (loadHyphenator (languageAffix Oriya))
panjabi = unsafePerformIO (loadHyphenator (languageAffix Panjabi))
piedmontese = unsafePerformIO (loadHyphenator (languageAffix Piedmontese))
polish = unsafePerformIO (loadHyphenator (languageAffix Polish))
portuguese = unsafePerformIO (loadHyphenator (languageAffix Portuguese))
romanian = unsafePerformIO (loadHyphenator (languageAffix Romanian))
romansh = unsafePerformIO (loadHyphenator (languageAffix Romansh))
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
thai = unsafePerformIO (loadHyphenator (languageAffix Thai))
turkish = unsafePerformIO (loadHyphenator (languageAffix Turkish))
turkmen = unsafePerformIO (loadHyphenator (languageAffix Turkmen))
ukrainian = unsafePerformIO (loadHyphenator (languageAffix Ukrainian))
uppersorbian = unsafePerformIO (loadHyphenator (languageAffix Uppersorbian))
welsh = unsafePerformIO (loadHyphenator (languageAffix Welsh))

-- | Load (and cache) the hyphenator for a given language.
languageHyphenator :: Language -> Hyphenator
languageHyphenator s = case s of
  Afrikaans -> afrikaans
  Armenian -> armenian
  Assamese -> assamese
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
  Georgian -> georgian
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
  Latin -> latin
  Latin_Classic -> latin_Classic
  Latvian -> latvian
  Lithuanian -> lithuanian
  Malayalam -> malayalam
  Marathi -> marathi
  Mongolian -> mongolian
  Norwegian_Bokmal  -> norwegian_Bokmal
  Norwegian_Nynorsk -> norwegian_Nynorsk
  Occitan -> occitan
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
