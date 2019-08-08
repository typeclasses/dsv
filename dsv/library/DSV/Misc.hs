{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Miscellania

module DSV.Misc
  ( byteStringDecimalRationalMaybe
  , textDecimalRationalMaybe
  , byteStringDollarsMaybe
  , textDollarsMaybe
  ) where

import DSV.ByteString
import DSV.Prelude
import DSV.Text
import DSV.UTF8

-- base
import Control.Monad ((>=>))

{- | Read a dollar amount.

=== Examples

  * @byteStringDollarsMaybe "$1234.567" = Just (1234567 % 1000)@
  * @byteStringDollarsMaybe "1234.567"  = Nothing@

-}

byteStringDollarsMaybe :: ByteString -> Maybe Rational
byteStringDollarsMaybe =
    utf8TextMaybe >=> textDollarsMaybe

{- | Read a dollar amount.

=== Examples

  * @byteStringDollarsMaybe "$1234.567" = Just (1234567 % 1000)@
  * @byteStringDollarsMaybe "1234.567"  = Nothing@

-}

textDollarsMaybe :: Text -> Maybe Rational
textDollarsMaybe =
    textStripPrefix "$" >=> textDecimalRationalMaybe

{- |

Read a rational number written in decimal notation.

=== Examples

  * @textDecimalRationalMaybe "1234"     = Just (1234 % 1)@
  * @textDecimalRationalMaybe "1234.567" = Just (1234567 % 1000)@
  * @textDecimalRationalMaybe "12.3.4"   = Nothing@

-}

textDecimalRationalMaybe :: Text -> Maybe Rational
textDecimalRationalMaybe =
    textReadMaybe textReadRational

{- |

Read a rational number written in decimal notation.

=== Examples

  * @textDecimalRationalMaybe "1234"     = Just (1234 % 1)@
  * @textDecimalRationalMaybe "1234.567" = Just (1234567 % 1000)@
  * @textDecimalRationalMaybe "12.3.4"   = Nothing@

-}

byteStringDecimalRationalMaybe :: ByteString -> Maybe Rational
byteStringDecimalRationalMaybe =
    utf8TextMaybe >=> textDecimalRationalMaybe
