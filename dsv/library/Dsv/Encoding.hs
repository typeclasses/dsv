{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Dsv.Encoding
  ( EncodeUtf8 (..)
  , DecodeUtf8 (..)
  , InvalidUtf8 (..)
  , utf8View
  ) where

import Dsv.ByteString
import Dsv.IO
import Dsv.Prelude
import Dsv.Text
import Dsv.Validation
import Dsv.ViewType


--- Encode ---

class EncodeUtf8 a
  where
    encodeUtf8 :: a -> ByteString

instance EncodeUtf8 Text
  where
    encodeUtf8 = textEncodeUtf8

instance EncodeUtf8 [Char]
  where
    encodeUtf8 = encodeUtf8 . stringToText


--- Decode ---

class DecodeUtf8 a
  where
    decodeUtf8Maybe :: ByteString -> Maybe a

instance DecodeUtf8 Text
  where
    decodeUtf8Maybe = textDecodeUtf8Maybe

instance DecodeUtf8 [Char]
  where
    decodeUtf8Maybe = fmap textToString . textDecodeUtf8Maybe


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
