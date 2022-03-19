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
-- Copyright   :  (C) 2012-2019 Edward Kmett,
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

-- $setup
-- >>> import Text.Hyphenation.Hyphenator

chrLine :: String -> [(Int, Char)]
chrLine (x:xs) = fmap (\y -> (fromEnum y, x)) xs
chrLine [] = []

-- | Read a built-in language file from the data directory where cabal installed this package.
--
-- (e.g. @hyphenateLanguage \"en-us\"@ opens @\"\/Users\/ekmett\/.cabal\/share\/hyphenation-0.2\/ghc-7.4.1\/hyph-en-us.hyp.txt\"@
-- among others when run on the author's local machine)
loadHyphenator :: Language -> IO Hyphenator
#if !EMBED
loadHyphenator language = do
  let affix = languageAffix language
  hyp <- unzipUtf8 <$> (getDataFileName ("hyph-" ++ affix ++ ".hyp.txt.gz") >>= Lazy.readFile)
  pat <- unzipUtf8 <$> (getDataFileName ("hyph-" ++ affix ++ ".pat.txt.gz") >>= Lazy.readFile)
  chr <- unzipUtf8 <$> (getDataFileName ("hyph-" ++ affix ++ ".chr.txt.gz") >>= Lazy.readFile)
  let chrMap = IM.fromList (Prelude.lines chr >>= chrLine)
      tryLookup x = IM.findWithDefault x (fromEnum x) chrMap
      (defaultLeftMin, defaultRightMin) = languageMins language
  return $ Hyphenator tryLookup (parsePatterns pat) (parseExceptions hyp) defaultLeftMin defaultRightMin
#else
loadHyphenator language = return $ Hyphenator tryLookup (parsePatterns pat) (parseExceptions hyp) defaultLeftMin defaultRightMin
  where affix = languageAffix language
        lookupHyphenatorFile fileName =
          case lookup fileName hyphenatorFiles of
            Just contents -> contents
            Nothing       -> error $ "Could not find " ++ fileName
        hyp = unzipUtf8 $ Lazy.fromStrict $ lookupHyphenatorFile $ "hyph-" ++ affix ++ ".hyp.txt.gz"
        pat = unzipUtf8 $ Lazy.fromStrict $ lookupHyphenatorFile $ "hyph-" ++ affix ++ ".pat.txt.gz"
        chr = unzipUtf8 $ Lazy.fromStrict $ lookupHyphenatorFile $ "hyph-" ++ affix ++ ".chr.txt.gz"
        chrMap = IM.fromList (Prelude.lines chr >>= chrLine)
        (defaultLeftMin, defaultRightMin) = languageMins language
        tryLookup x = IM.findWithDefault x (fromEnum x) chrMap
#endif

unzipUtf8 :: ByteString -> String
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


-- | The number of characters from the beginning and end of a word not to hyphenate in this language.
languageMins :: Language -> (Int, Int)
languageMins s = case s of
  Afrikaans -> (1, 2)
  Armenian -> (1, 2)
  Assamese -> (1, 1)
  Basque -> (2, 2)
  Bengali -> (1, 1)
  Bulgarian -> (2, 2)
  Catalan -> (2, 2)
  Chinese -> (1, 1)
  Coptic -> (1, 1)
  Croatian -> (2, 2)
  Czech -> (2, 3)
  Danish -> (2, 2)
  Dutch -> (2, 2)
  English_GB -> (2, 3)
  English_US -> (2, 3)
  Esperanto -> (2, 2)
  Estonian -> (2, 3)
  Ethiopic -> (1, 1)
  -- Farsi -> (,)
  Finnish -> (2, 2)
  French -> (2, 3)
  Friulan -> (2, 2)
  Galician -> (2, 2)
  Georgian -> (1, 2)
  German_1901 -> (2, 2)
  German_1996 -> (2, 2)
  German_Swiss -> (2, 2)
  Greek_Ancient -> (1, 1)
  Greek_Mono -> (1, 1)
  Greek_Poly -> (1, 1)
  Gujarati -> (1, 1)
  Hindi -> (1, 1)
  Hungarian -> (2, 2)
  Icelandic -> (2, 2)
  Indonesian -> (2, 2)
  Interlingua -> (2, 2)
  Irish -> (2, 3)
  Italian -> (2, 2)
  Kannada -> (1, 1)
  Kurmanji -> (2, 2)
  Latin -> (2, 2)
  Latin_Classic -> (2, 2)
  Latvian -> (2, 2)
  Lithuanian -> (2, 2)
  Malayalam -> (1, 1)
  Marathi -> (1, 1)
  Mongolian -> (2, 2)
  Norwegian_Bokmal -> (2, 2)
  Norwegian_Nynorsk -> (2, 2)
  Occitan -> (2, 2)
  Oriya -> (1, 1)
  Panjabi -> (1, 1)
  Piedmontese -> (2, 2)
  Polish -> (2, 2)
  Portuguese -> (2, 3)
  Romanian -> (2, 2)
  Romansh -> (2, 2)
  Russian -> (2, 2)
  Sanskrit -> (1, 3)
  Serbian_Cyrillic -> (2, 2)
  Serbocroatian_Cyrillic -> (2, 2)
  Serbocroatian_Latin -> (2, 2)
  Slovak -> (2, 3)
  Slovenian -> (2, 2)
  Spanish -> (2, 2)
  Swedish -> (2, 2)
  Tamil -> (1, 1)
  Telugu -> (1, 1)
  Thai -> (2, 3)
  Turkish -> (2, 2)
  Turkmen -> (2, 2)
  Ukrainian -> (2, 2)
  Uppersorbian -> (2, 2)
  Welsh -> (2, 3)


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

afrikaans = unsafePerformIO (loadHyphenator Afrikaans)
armenian = unsafePerformIO (loadHyphenator Armenian)
assamese = unsafePerformIO (loadHyphenator Assamese)
basque = unsafePerformIO (loadHyphenator Basque)
bengali = unsafePerformIO (loadHyphenator Bengali)
bulgarian = unsafePerformIO (loadHyphenator Bulgarian)
catalan = unsafePerformIO (loadHyphenator Catalan)
chinese = unsafePerformIO (loadHyphenator Chinese)
coptic = unsafePerformIO (loadHyphenator Coptic)
croatian = unsafePerformIO (loadHyphenator Croatian)
czech = unsafePerformIO (loadHyphenator Czech)
danish = unsafePerformIO (loadHyphenator Danish)
dutch = unsafePerformIO (loadHyphenator Dutch)
english_US = unsafePerformIO (loadHyphenator English_US)
english_GB = unsafePerformIO (loadHyphenator English_GB)
esperanto = unsafePerformIO (loadHyphenator Esperanto)
estonian = unsafePerformIO (loadHyphenator Estonian)
ethiopic = unsafePerformIO (loadHyphenator Ethiopic)
-- farsi = unsafePerformIO (loadHyphenator Farsi)
finnish = unsafePerformIO (loadHyphenator Finnish)
french = unsafePerformIO (loadHyphenator French)
friulan = unsafePerformIO (loadHyphenator Friulan)
galician = unsafePerformIO (loadHyphenator Galician)
georgian = unsafePerformIO (loadHyphenator Georgian)
german_1901 = unsafePerformIO (loadHyphenator German_1901)
german_1996 = unsafePerformIO (loadHyphenator German_1996)
german_Swiss = unsafePerformIO (loadHyphenator German_Swiss)
greek_Ancient = unsafePerformIO (loadHyphenator Greek_Ancient)
greek_Mono = unsafePerformIO (loadHyphenator Greek_Mono)
greek_Poly = unsafePerformIO (loadHyphenator Greek_Poly)
gujarati = unsafePerformIO (loadHyphenator Gujarati)
hindi = unsafePerformIO (loadHyphenator Hindi)
hungarian = unsafePerformIO (loadHyphenator Hungarian)
icelandic = unsafePerformIO (loadHyphenator Icelandic)
indonesian = unsafePerformIO (loadHyphenator Indonesian)
interlingua = unsafePerformIO (loadHyphenator Interlingua)
irish = unsafePerformIO (loadHyphenator Irish)
italian = unsafePerformIO (loadHyphenator Italian)
kannada = unsafePerformIO (loadHyphenator Kannada)
kurmanji = unsafePerformIO (loadHyphenator Kurmanji)
latin = unsafePerformIO (loadHyphenator Latin)
latin_Classic = unsafePerformIO (loadHyphenator Latin_Classic)
latvian = unsafePerformIO (loadHyphenator Latvian)
lithuanian = unsafePerformIO (loadHyphenator Lithuanian)
malayalam = unsafePerformIO (loadHyphenator Malayalam)
marathi = unsafePerformIO (loadHyphenator Marathi)
mongolian = unsafePerformIO (loadHyphenator Mongolian)
norwegian_Bokmal = unsafePerformIO (loadHyphenator Norwegian_Bokmal)
norwegian_Nynorsk = unsafePerformIO (loadHyphenator Norwegian_Nynorsk)
occitan = unsafePerformIO (loadHyphenator Occitan)
oriya = unsafePerformIO (loadHyphenator Oriya)
panjabi = unsafePerformIO (loadHyphenator Panjabi)
piedmontese = unsafePerformIO (loadHyphenator Piedmontese)
polish = unsafePerformIO (loadHyphenator Polish)
portuguese = unsafePerformIO (loadHyphenator Portuguese)
romanian = unsafePerformIO (loadHyphenator Romanian)
romansh = unsafePerformIO (loadHyphenator Romansh)
russian = unsafePerformIO (loadHyphenator Russian)
sanskrit = unsafePerformIO (loadHyphenator Sanskrit)
serbian_Cyrillic = unsafePerformIO (loadHyphenator Serbian_Cyrillic)
serbocroatian_Cyrillic = unsafePerformIO (loadHyphenator Serbocroatian_Cyrillic)
serbocroatian_Latin = unsafePerformIO (loadHyphenator Serbocroatian_Latin)
slovak = unsafePerformIO (loadHyphenator Slovak)
slovenian = unsafePerformIO (loadHyphenator Slovenian)
spanish = unsafePerformIO (loadHyphenator Spanish)
swedish = unsafePerformIO (loadHyphenator Swedish)
tamil = unsafePerformIO (loadHyphenator Tamil)
telugu = unsafePerformIO (loadHyphenator Telugu)
thai = unsafePerformIO (loadHyphenator Thai)
turkish = unsafePerformIO (loadHyphenator Turkish)
turkmen = unsafePerformIO (loadHyphenator Turkmen)
ukrainian = unsafePerformIO (loadHyphenator Ukrainian)
uppersorbian = unsafePerformIO (loadHyphenator Uppersorbian)
welsh = unsafePerformIO (loadHyphenator Welsh)

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
