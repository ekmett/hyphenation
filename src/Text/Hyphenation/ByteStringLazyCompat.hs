{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation.ByteStringLazyCompat
-- Copyright   :  (C) 2012-2019 Edward Kmett,
--                (C) 2007 Ned Batchelder
-- License     :  BSD-style (see the languageAffix LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Text.Hyphenation.ByteStringLazyCompat
  ( module Lazy
#if !(MIN_VERSION_bytestring(0,10,0))
  , fromStrict
#endif
  ) where

import Data.ByteString.Lazy as Lazy

#if !(MIN_VERSION_bytestring(0,10,0))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (ByteString(..))

-- |/O(1)/ Convert a strict 'ByteString' into a lazy 'ByteString'.
fromStrict :: BS.ByteString -> ByteString
fromStrict bs | BS.null bs = Empty
              | otherwise = Chunk bs Empty
#endif
