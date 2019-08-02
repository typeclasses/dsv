{-# LANGUAGE FlexibleInstances, NoImplicitPrelude #-}

module Dsv.Encoding
  ( EncodeUtf8 (..)
  , DecodeUtf8 (..)
  ) where

import Dsv.ByteString
import Dsv.Prelude
import Dsv.Text


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
