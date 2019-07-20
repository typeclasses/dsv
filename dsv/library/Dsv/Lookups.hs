{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Dsv.Lookups
  ( column, columnUtf8, columnN, entireRow
  , mapLookup
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

mapLookup ::
    forall he re a b .
    (a -> Validation re b)
    -> Lookup he re a
    -> Lookup he re b

mapLookup g (Lookup f) =
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

column ::
    forall he re .
    (DuplicateColumn ByteString he, MissingColumn ByteString he, RowTooShort re)
    => ByteString -> Lookup he re ByteString

column name =
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

columnUtf8 ::
    forall he re txt .
    (EncodeUtf8 txt, DecodeUtf8 txt, DuplicateColumn txt he, MissingColumn txt he, RowTooShort re, FieldInvalidUtf8 txt re)
    => txt -> Lookup he re txt

columnUtf8 name =
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

columnN ::
    forall he re .
    RowTooShort re
    => Integer
    -> Lookup he re ByteString

columnN n =
  Lookup
    (\_header ->
        Success
            (\row ->
                case vectorIndexInteger row (n - 1) of
                    Nothing -> Failure rowTooShort
                    Just x -> Success x
            )
    )

entireRow :: forall he re . Lookup he re (Vector ByteString)
entireRow = Lookup (\_header -> Success (\row -> Success row))
