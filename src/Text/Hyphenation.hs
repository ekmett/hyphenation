-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Hyphenation
-- Copyright   :  (C) 2012-2019 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Hyphenation based on the Knuth-Liang algorithm as used by TeX.
--
-- The implementation is based on Ned Batchelder's public domain @hyphenate.py@
-- and simplified to remove the need for a manual exception list.
----------------------------------------------------------------------------
module Text.Hyphenation
  ( module Text.Hyphenation.Hyphenator
  , module Text.Hyphenation.Language
  ) where

import Text.Hyphenation.Hyphenator
import Text.Hyphenation.Language

