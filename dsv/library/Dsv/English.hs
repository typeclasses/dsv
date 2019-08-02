{-# LANGUAGE DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}

module Dsv.English
  ( EnglishText (..)
  ) where

import Dsv.LookupErrors
import Dsv.Prelude
import Dsv.Text

newtype EnglishText = EnglishText [Text]
    deriving stock (Eq, Show)
    deriving newtype Semigroup

instance DsvError RowTooShort EnglishText
  where
    dsvError RowTooShort =
      EnglishText ["Row does not have enough columns."]

instance DsvError (DuplicateColumn Text) EnglishText
  where
    dsvError (DuplicateColumn name) =
      EnglishText ["The column name '" <> name <> "' appears more than once in the header."]

instance DsvError (MissingColumn Text) EnglishText
  where
    dsvError (MissingColumn name) =
      EnglishText ["The column name '" <> name <> "' is not present in the header."]

instance DsvError (InvalidUtf8 Text) EnglishText
  where
    dsvError (InvalidUtf8 name) =
      EnglishText ["The byte string in column named '" <> name <> "' is not valid UTF-8."]
