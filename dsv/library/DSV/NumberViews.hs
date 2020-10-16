{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DSV.NumberViews
  ( InvalidNat (..), byteStringNatView, textNatView
                   , byteStringNatView_, textNatView_
  , InvalidRational (..), byteStringRationalView, textRationalView
                        , byteStringRationalView_, textRationalView_
  , InvalidDollars (..), byteStringDollarsView, textDollarsView
                       , byteStringDollarsView_, textDollarsView_
  ) where

import DSV.AttoView
import DSV.ByteString
import DSV.IO
import DSV.Numbers
import DSV.Prelude
import DSV.Text
import DSV.TextReaderView
import DSV.UTF8
import DSV.Validation
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

byteStringNatView_ :: View () ByteString Natural
byteStringNatView_ = discardViewError byteStringNatView

textNatView :: View InvalidNat Text Natural
textNatView = textReaderView InvalidNat textReadDecimal

textNatView_ :: View () Text Natural
textNatView_ = discardViewError textNatView

data InvalidRational = InvalidRational
  deriving stock (Eq, Show)
  deriving anyclass Exception

{- |

Read a rational number written in decimal notation.

=== Examples

>>> :set -XOverloadedStrings

>>> applyView byteStringRationalView "1234"
Success (1234 % 1)

>>> applyView byteStringRationalView "1234.567"
Success (1234567 % 1000)

>>> applyView byteStringRationalView "12.3.4"
Failure InvalidRational

-}

byteStringRationalView :: View InvalidRational ByteString Rational
byteStringRationalView =
    textRationalView .
    overViewError (\InvalidUtf8 -> InvalidRational) utf8TextView

byteStringRationalView_ :: View () ByteString Rational
byteStringRationalView_ = discardViewError byteStringRationalView

{- |

Read a rational number written in decimal notation.

=== Examples

>>> :set -XOverloadedStrings

>>> applyView textRationalView "1234"
Success (1234 % 1)

>>> applyView textRationalView "1234.567"
Success (1234567 % 1000)

>>> applyView textRationalView "12.3.4"
Failure InvalidRational

-}

textRationalView :: View InvalidRational Text Rational
textRationalView = textReaderView InvalidRational textReadRational

textRationalView_ :: View () Text Rational
textRationalView_ = discardViewError textRationalView

data InvalidDollars = InvalidDollars
  deriving stock (Eq, Show)
  deriving anyclass Exception

{- | Read a dollar amount.

=== Examples

>>> applyView byteStringDollarsView "$1234.567"
Success (1234567 % 1000)

>>> applyView byteStringDollarsView "1234.567"
Failure InvalidDollars

-}

byteStringDollarsView :: View InvalidDollars ByteString Rational
byteStringDollarsView =
    textDollarsView .
    overViewError (\InvalidUtf8 -> InvalidDollars) utf8TextView

byteStringDollarsView_ :: View () ByteString Rational
byteStringDollarsView_ = discardViewError byteStringDollarsView

{- | Read a dollar amount.

=== Examples

>>> applyView textDollarsView "$1234.567"
Success (1234567 % 1000)

>>> applyView textDollarsView "1234.567"
Failure InvalidDollars

-}

textDollarsView :: View InvalidDollars Text Rational
textDollarsView =
    overViewError
        (\InvalidRational -> InvalidDollars)
        textRationalView
    .
    View (
        maybe (Failure InvalidDollars) Success .
        textStripPrefix "$"
    )

textDollarsView_ :: View () Text Rational
textDollarsView_ = discardViewError textDollarsView
