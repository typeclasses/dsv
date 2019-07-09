{-# LANGUAGE OverloadedStrings #-}

-- | Miscellania

module Pipes.Dsv.Misc
  ( byteStringTextUtf8Maybe
  , byteStringRationalMaybe
  , textRationalMaybe
  , byteStringDollarsMaybe
  , textDollarsMaybe
  ) where

import Pipes.Dsv.ByteString

-- base
import Control.Monad ((>=>))

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text

rightMaybe :: Either a b -> Maybe b
rightMaybe =
    either (const Nothing) Just

-- | Decode a byte string as UTF-8 text, failing with 'Nothing' if the decoding fails.

byteStringTextUtf8Maybe :: ByteString -> Maybe Text
byteStringTextUtf8Maybe =
    rightMaybe . Text.decodeUtf8'

textReadMaybe :: Text.Reader a -> Text -> Maybe a
textReadMaybe f t =
    case f t of
        Right (x, r) | Text.null r -> Just x
        _ -> Nothing

{- | Read a dollar amount.

  * @byteStringDollarsMaybe "$1234.567" = Just (1234567 % 1000)@

  * @byteStringDollarsMaybe "1234.567" = Nothing@

-}

byteStringDollarsMaybe :: ByteString -> Maybe Rational
byteStringDollarsMaybe =
    byteStringTextUtf8Maybe >=> textDollarsMaybe

textDollarsMaybe :: Text -> Maybe Rational
textDollarsMaybe =
    Text.stripPrefix "$" >=> textRationalMaybe

textRationalMaybe :: Text -> Maybe Rational
textRationalMaybe =
    textReadMaybe Text.rational

byteStringRationalMaybe :: ByteString -> Maybe Rational
byteStringRationalMaybe =
    byteStringTextUtf8Maybe >=> textRationalMaybe
