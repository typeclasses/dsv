{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module DSV.NumberViews
  ( InvalidNat (..), byteStringNatView, textNatView
  ) where

import DSV.AttoView
import DSV.ByteString
import DSV.IO
import DSV.Numbers
import DSV.Prelude
import DSV.Text
import DSV.TextReaderView
import DSV.ViewType

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
