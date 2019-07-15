{-# LANGUAGE FlexibleContexts #-}

module Dsv.Lookups
  ( column, columnUtf8, columnN, entireRow
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

column
    :: (DuplicateColumn ByteString he, MissingColumn ByteString he, RowTooShort re)
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

columnUtf8
    :: (EncodeUtf8 txt, DuplicateColumn txt he, MissingColumn txt he, RowTooShort re)
    => txt -> Lookup he re ByteString

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
                            Just x -> Success x
                    )
            _   -> Failure (duplicateColumn name)

    )

columnN
    :: RowTooShort re
    => Integer -> Lookup he re ByteString

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

entireRow :: Lookup he re (Vector ByteString)
entireRow = Lookup (\_header -> Success (\row -> Success row))
