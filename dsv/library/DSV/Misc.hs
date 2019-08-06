{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Miscellania

module DSV.Misc
  ( byteStringTextUtf8Maybe
  , byteStringDecimalRationalMaybe
  , textDecimalRationalMaybe
  , byteStringDollarsMaybe
  , textDollarsMaybe
  ) where

import DSV.ByteString
import DSV.Prelude
import DSV.Text
import DSV.Vector

-- base
import Control.Monad ((>=>))

-- vector
import qualified Data.Vector as Vector

-- | Decode a byte string as UTF-8 text, failing with 'Nothing' if the decoding fails.

byteStringTextUtf8Maybe :: ByteString -> Maybe Text
byteStringTextUtf8Maybe = textDecodeUtf8Maybe

{- | Read a dollar amount.

=== Examples

  * @byteStringDollarsMaybe "$1234.567" = Just (1234567 % 1000)@
  * @byteStringDollarsMaybe "1234.567"  = Nothing@

-}

byteStringDollarsMaybe :: ByteString -> Maybe Rational
byteStringDollarsMaybe =
    byteStringTextUtf8Maybe >=> textDollarsMaybe

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
    byteStringTextUtf8Maybe >=> textDecimalRationalMaybe
