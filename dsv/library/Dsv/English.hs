{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Dsv.English
  ( EnglishText (..)
  , ToEnglishText (..)
  , englishLookupError
  ) where

import Dsv.IO
import Dsv.LookupErrors
import Dsv.LookupType
--import Dsv.Position
import Dsv.Prelude
import Dsv.Text

newtype EnglishText = EnglishText [Text]
    deriving stock (Eq, Show)
    deriving newtype Semigroup

instance Exception EnglishText
  where
    displayException (EnglishText xs) = unwords (map textToString xs)

class ToEnglishText a
  where
    toEnglishText :: a -> EnglishText

englishLookupError ::
    (ToEnglishText he, ToEnglishText re) =>
    Lookup he re a -> Lookup EnglishText EnglishText a

englishLookupError =
  mapLookupError toEnglishText toEnglishText

instance ToEnglishText TooShort
  where
    toEnglishText TooShort =
      EnglishText ["Row does not have enough columns."]

{-
instance ToEnglishText (DuplicateColumn Text)
  where
    toEnglishText Duplicate =
      EnglishText ["The column name '" <> name <> "' appears more than once in the header."]

instance LiftError (MissingColumn Text) EnglishText
  where
    liftError (MissingColumn name) =
      EnglishText ["The column name '" <> name <> "' is not present in the header."]

instance LiftError (InvalidUtf8 Text) EnglishText
  where
    liftError (InvalidUtf8 name) =
      EnglishText ["The content of the column named '" <> name <> "' is not valid UTF-8."]

instance LiftError (InvalidNat Text) EnglishText
  where
    liftError (InvalidNat name) =
      EnglishText ["The content of the column named '" <> name <> "' must consist of one or more characters between 0 and 9."]

-}
