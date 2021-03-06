{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Text
  ( Text
  , stringToText, textToString
  , textNull, textStripPrefix
  , TextReader, textReadMaybe, textReadRational, textReadDecimal
  ) where

import DSV.Numbers
import DSV.Prelude

-- text
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.Read

stringToText :: String -> Text
stringToText = Data.Text.pack

textToString :: Text -> String
textToString = Data.Text.unpack

textNull :: Text -> Bool
textNull = Data.Text.null

textStripPrefix :: Text -> Text -> Maybe Text
textStripPrefix = Data.Text.stripPrefix

type TextReader a = Data.Text.Read.Reader a

textReadMaybe ::
    forall a .
    TextReader a -> Text -> Maybe a

textReadMaybe f t =
    case f t of
        Right (x, r) | textNull r -> Just x
        _ -> Nothing

textReadRational :: TextReader Rational
textReadRational = Data.Text.Read.rational

textReadDecimal :: TextReader Natural
textReadDecimal = Data.Text.Read.decimal
