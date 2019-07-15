{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings #-}

module Dsv.English
  ( EnglishText (..)
  ) where

import Dsv.LookupErrors
import Dsv.Text

newtype EnglishText = EnglishText [Text]
    deriving stock (Eq, Show)
    deriving newtype Semigroup

instance RowTooShort EnglishText
  where
    rowTooShort = EnglishText ["Row does not have enough columns."]

instance DuplicateColumn Text EnglishText
  where
    duplicateColumn name = EnglishText ["The column name '" <> name <> "' appears more than once in the header."]

instance MissingColumn Text EnglishText
  where
    missingColumn name = EnglishText ["The column name '" <> name <> "' is not present in the header."]

instance FieldInvalidUtf8 Text EnglishText
  where
    fieldInvalidUtf8 name = EnglishText ["The byte string in column named '" <> name <> "' is not valid UTF-8."]
