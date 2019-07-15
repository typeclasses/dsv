module Dsv.Text
  ( Text
  , textEncodeUtf8, textDecodeUtf8Maybe
  , stringToText, textToString
  , textNull, textStripPrefix
  , TextReader, textReadRational
  ) where

import Dsv.ByteString

-- text
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Read

textDecodeUtf8Maybe :: ByteString -> Maybe Text
textDecodeUtf8Maybe = rightMaybe . Data.Text.Encoding.decodeUtf8'

textEncodeUtf8 :: Text -> ByteString
textEncodeUtf8 = Data.Text.Encoding.encodeUtf8

stringToText :: String -> Text
stringToText = Data.Text.pack

textToString :: Text -> String
textToString = Data.Text.unpack

rightMaybe :: Either a b -> Maybe b
rightMaybe = either (const Nothing) Just

textNull :: Text -> Bool
textNull = Data.Text.null

textStripPrefix :: Text -> Text -> Maybe Text
textStripPrefix = Data.Text.stripPrefix

type TextReader a = Data.Text.Read.Reader a

textReadRational :: TextReader Rational
textReadRational = Data.Text.Read.rational
