{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.NumberViews
  ( InvalidNat (..), byteStringNatView, textNatView
  ) where

import Dsv.AttoView
import Dsv.ByteString
import Dsv.IO
import Dsv.Numbers
import Dsv.Prelude
import Dsv.Text
import Dsv.TextReaderView
import Dsv.ViewType

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8

data InvalidNat = InvalidNat
  deriving stock (Eq, Show)
  deriving anyclass Exception

byteStringNatView :: View InvalidNat ByteString Natural
byteStringNatView = attoByteStringView InvalidNat p
  where
    p = Data.Attoparsec.ByteString.Char8.decimal

textNatView :: View InvalidNat Text Natural
textNatView = textReaderView InvalidNat textReadDecimal
