{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.Lookups
  ( byteStringLookup, textLookupUtf8, byteStringLookupPosition, entireRowLookup
  , refineLookup
  ) where

import Dsv.ByteString
import Dsv.Encoding
import Dsv.LookupErrors
import Dsv.LookupType
import Dsv.Prelude
import Dsv.Readings
import Dsv.ReadingType
import Dsv.Validation
import Dsv.Vector

-- base
import qualified Data.Foldable as Foldable
import qualified Data.List as List

refineLookup ::
    forall he re a b .
    Lookup he re a
    -> Reading re a b
    -> Lookup he re b

refineLookup (Lookup (Reading f)) r2 =
  Lookup $
    Reading $
      fmap (r2 .) . f

byteStringLookup ::
    forall he re .
    (DuplicateColumn ByteString he, MissingColumn ByteString he, RowTooShort re)
    => ByteString -> Lookup he re ByteString

byteStringLookup name =
  Lookup $
    Reading $
      \header ->
          case List.findIndices (== name) (Foldable.toList header) of
              []  -> Failure (missingColumn name)
              [i] ->
                  Success $
                    Reading $
                      \row ->
                          case vectorIndexInt row i of
                              Nothing -> Failure rowTooShort
                              Just x -> Success x
              _   -> Failure (duplicateColumn name)

textLookupUtf8 ::
    forall he re txt .
    (EncodeUtf8 txt, DecodeUtf8 txt, DuplicateColumn txt he, MissingColumn txt he, RowTooShort re, FieldInvalidUtf8 txt re)
    => txt -> Lookup he re txt

textLookupUtf8 name =
  Lookup $
    Reading $
      \header ->
        case List.findIndices (== encodeUtf8 name) (Foldable.toList header) of
            []  -> Failure (missingColumn name)
            [i] ->
                Success $
                  Reading $
                    \row ->
                        case vectorIndexInt row i of
                            Nothing -> Failure rowTooShort
                            Just x ->
                                case decodeUtf8Maybe x of
                                    Nothing -> Failure (fieldInvalidUtf8 name)
                                    Just y -> Success y
            _   -> Failure (duplicateColumn name)

byteStringLookupPosition ::
    forall he re .
    RowTooShort re
    => Integer
    -> Lookup he re ByteString

byteStringLookupPosition n =
  Lookup $
    Reading $
      \_header ->
        Success $
          Reading $
            \row ->
                case vectorIndexInteger row (n - 1) of
                    Nothing -> Failure rowTooShort
                    Just x -> Success x

entireRowLookup :: forall he re . Lookup he re (Vector ByteString)
entireRowLookup = Lookup (constReading id)
