{-# LANGUAGE FlexibleInstances #-}

module Dsv.Encoding
  ( EncodeUtf8 (..)
  ) where

import Dsv.ByteString
import Dsv.Text

class EncodeUtf8 a
  where
    encodeUtf8 :: a -> ByteString

instance EncodeUtf8 Text
  where
    encodeUtf8 = textEncodeUtf8

instance EncodeUtf8 [Char]
  where
    encodeUtf8 = encodeUtf8 . stringToText
