{-# LANGUAGE TemplateHaskell #-}

module Pipes.Dsv.Delimiter (Delimiter (..), charDelimiter, comma, tab, delimiterWord8) where

import Pipes.Dsv.Delimiter.Type
import Pipes.Dsv.Delimiter.TH

import Data.Word (Word8)

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

delimiterWord8 :: Delimiter -> Word8
delimiterWord8 (Delimiter d) = d
