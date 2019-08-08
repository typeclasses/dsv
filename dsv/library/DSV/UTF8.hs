{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module DSV.UTF8
  ( EncodeUtf8 (..)
  , DecodeUtf8 (..)
  , InvalidUtf8 (..)
  , utf8View, utf8TextView, encodeTextUtf8, utf8TextMaybe
  ) where

import DSV.ByteString
import DSV.IO
import DSV.Prelude
import DSV.Validation
import DSV.ViewType

-- text
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.Encoding


--- Encode ---

class EncodeUtf8 a
  where
    encodeUtf8 :: a -> ByteString

instance EncodeUtf8 Text
  where
    encodeUtf8 =
        Data.Text.Encoding.encodeUtf8

instance EncodeUtf8 [Char]
  where
    encodeUtf8 =
        Data.Text.Encoding.encodeUtf8 .
        Data.Text.pack


--- Decode ---

class DecodeUtf8 a
  where
    decodeUtf8Maybe :: ByteString -> Maybe a

instance DecodeUtf8 Text
  where
    decodeUtf8Maybe =
        rightMaybe .
        Data.Text.Encoding.decodeUtf8'

instance DecodeUtf8 [Char]
  where
    decodeUtf8Maybe =
        fmap Data.Text.unpack .
        rightMaybe .
        Data.Text.Encoding.decodeUtf8'


--- Views ---

data InvalidUtf8 = InvalidUtf8
  deriving stock (Eq, Show)
  deriving anyclass Exception

utf8View :: DecodeUtf8 txt => View InvalidUtf8 ByteString txt
utf8View =
  View $ \x ->
    case decodeUtf8Maybe x of
        Nothing -> Failure InvalidUtf8
        Just y -> Success y

-- | Decode a byte string as UTF-8 text, failing with 'Nothing' if the decoding fails.
utf8TextMaybe :: ByteString -> Maybe Text
utf8TextMaybe = decodeUtf8Maybe @Text

utf8TextView :: View InvalidUtf8 ByteString Text
utf8TextView = utf8View @Text

encodeTextUtf8 :: Text -> ByteString
encodeTextUtf8 = encodeUtf8 @Text

rightMaybe :: Either a b -> Maybe b
rightMaybe = either (const Nothing) Just
