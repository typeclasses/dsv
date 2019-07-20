{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Dsv.Lookups
  ( byteStringLookup, textLookupUtf8, byteStringLookupPosition, entireRowLookup
  , lookupRead
  ) where

import Dsv.ByteString
import Dsv.Encoding
import Dsv.LookupErrors
import Dsv.LookupType
import Dsv.Validation
import Dsv.Vector

-- base
import qualified Data.Foldable as Foldable
import qualified Data.List as List

lookupRead ::
    forall he re a b .
    (a -> Validation re b)
    -> Lookup he re a
    -> Lookup he re b

lookupRead g (Lookup f) =
  Lookup (
    \header ->
      case f header of
        Failure e -> Failure e
        Success h ->
          Success
            (\row ->
              case h row of
                Failure e -> Failure e
                Success x -> g x
            )
  )

byteStringLookup ::
    forall he re .
    (DuplicateColumn ByteString he, MissingColumn ByteString he, RowTooShort re)
    => ByteString -> Lookup he re ByteString

byteStringLookup name =
  Lookup
    (\header ->
        case List.findIndices (== name) (Foldable.toList header) of
            []  -> Failure (missingColumn name)
            [i] ->
                Success
                    (\row ->
                        case vectorIndexInt row i of
                            Nothing -> Failure rowTooShort
                            Just x -> Success x
                    )
            _   -> Failure (duplicateColumn name)
    )

textLookupUtf8 ::
    forall he re txt .
    (EncodeUtf8 txt, DecodeUtf8 txt, DuplicateColumn txt he, MissingColumn txt he, RowTooShort re, FieldInvalidUtf8 txt re)
    => txt -> Lookup he re txt

textLookupUtf8 name =
  Lookup
    (\header ->
        case List.findIndices (== encodeUtf8 name) (Foldable.toList header) of
            []  -> Failure (missingColumn name)
            [i] ->
                Success
                    (\row ->
                        case vectorIndexInt row i of
                            Nothing -> Failure rowTooShort
                            Just x ->
                                case decodeUtf8Maybe x of
                                    Nothing -> Failure (fieldInvalidUtf8 name)
                                    Just y -> Success y
                    )
            _   -> Failure (duplicateColumn name)

    )

byteStringLookupPosition ::
    forall he re .
    RowTooShort re
    => Integer
    -> Lookup he re ByteString

byteStringLookupPosition n =
  Lookup
    (\_header ->
        Success
            (\row ->
                case vectorIndexInteger row (n - 1) of
                    Nothing -> Failure rowTooShort
                    Just x -> Success x
            )
    )

entireRowLookup :: forall he re . Lookup he re (Vector ByteString)
entireRowLookup = Lookup (\_header -> Success (\row -> Success row))
