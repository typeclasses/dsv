{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module DSV.CommonDelimiters
  ( Delimiter (..)
  , charDelimiter
  , delimiterWord8
  , comma, tab
  ) where

import DSV.DelimiterType
import DSV.DelimiterSplice

-- | ASCII code point @0x2C@, the typical choice of DSV delimiter. DSV (delimiter-separated value) files that use the comma delimiter are called CSV (comma-separated-value) files.
--
-- @comma = $('charDelimiter' ',')@
comma :: Delimiter
comma = $(charDelimiter ',')

-- | ASCII code point @0x09@, the "horizontal tab" character.
--
-- @tab = $('charDelimiter' '\t')@
tab :: Delimiter
tab = $(charDelimiter '\t')
