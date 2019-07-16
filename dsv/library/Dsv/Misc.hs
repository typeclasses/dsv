{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Miscellania

module Dsv.Misc
  ( nthColumn, columnName
  , byteStringTextUtf8Maybe
  , byteStringDecimalRationalMaybe
  , textDecimalRationalMaybe
  , byteStringDollarsMaybe
  , textDollarsMaybe
  ) where

import Dsv.ByteString
import Dsv.Text
import Dsv.Vector

-- base
import Control.Monad ((>=>))

-- vector
import qualified Data.Vector as Vector

{- |

=== Examples

  * @nthColumn 0 ["a", "b", "c"] = Nothing@
  * @nthColumn 1 ["a", "b", "c"] = Just "a"@
  * @nthColumn 2 ["a", "b", "c"] = Just "b"@
  * @nthColumn 3 ["a", "b", "c"] = Just "c"@
  * @nthColumn 4 ["a", "b", "c"] = Nothing@

-}

nthColumn :: forall a . Integer -> Vector a -> Maybe a
nthColumn n xs = vectorIndexInteger xs (n - 1)

columnName ::
    forall name value .
    Eq name
    => name
    -> Vector (name, value)
    -> Maybe value

columnName n xs =
  do
    (_, v) <- Vector.find (\(n', _) -> n == n') xs
    return v

-- | Decode a byte string as UTF-8 text, failing with 'Nothing' if the decoding fails.

byteStringTextUtf8Maybe :: ByteString -> Maybe Text
byteStringTextUtf8Maybe = textDecodeUtf8Maybe

textReadMaybe ::
    forall a .
    TextReader a -> Text -> Maybe a

textReadMaybe f t =
    case f t of
        Right (x, r) | textNull r -> Just x
        _ -> Nothing

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
