{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.Lookups
  ( byteStringLookup, textLookupUtf8, textLookupUtf8', byteStringLookupPosition, entireRowLookup
  , refineLookup, natLookupUtf8
  ) where

import Dsv.ByteString
import Dsv.Encoding
import Dsv.LookupErrors
import Dsv.LookupType
import Dsv.Numbers
import Dsv.Prelude
import Dsv.Validation
import Dsv.Vector
import Dsv.Views
import Dsv.ViewType

-- base
import qualified Data.Foldable as Foldable
import qualified Data.List as List

-- attoparsec
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8

refineLookup ::
    forall he re a b .
    Lookup he re a
    -> View re a b
    -> Lookup he re b

refineLookup (Lookup (View f)) r2 =
  Lookup $
    View $
      fmap (r2 .) . f

byteStringLookup ::
    forall he re .
    ( DsvError (DuplicateColumn ByteString) he
    , DsvError (MissingColumn ByteString) he
    , DsvError RowTooShort re
    )
    => ByteString -> Lookup he re ByteString

byteStringLookup name =
  Lookup $
    View $
      \header ->
          case List.findIndices (== name) (Foldable.toList header) of
              []  -> Failure (dsvError (MissingColumn name))
              [i] ->
                  Success $
                    View $
                      \row ->
                          case vectorIndexInt row i of
                              Nothing -> Failure (dsvError RowTooShort)
                              Just x -> Success x
              _   -> Failure (dsvError (DuplicateColumn name))

textLookupUtf8 ::
    forall he re txt .
    ( EncodeUtf8 txt, DecodeUtf8 txt
    , DsvError (DuplicateColumn txt) he
    , DsvError (MissingColumn txt) he
    , DsvError RowTooShort re
    , DsvError (InvalidUtf8 txt) re
    )
    => txt -> Lookup he re txt

textLookupUtf8 name =
  Lookup $
    View $
      \header ->
        case List.findIndices (== encodeUtf8 name) (Foldable.toList header) of
            []  -> Failure (dsvError (MissingColumn name))
            [i] ->
                Success $
                  View $
                    \row ->
                        case vectorIndexInt row i of
                            Nothing -> Failure (dsvError RowTooShort)
                            Just x ->
                                case decodeUtf8Maybe x of
                                    Nothing -> Failure (dsvError (InvalidUtf8 name))
                                    Just y -> Success y
            _   -> Failure (dsvError (DuplicateColumn name))

textLookupUtf8' ::
    forall he re txt .
    ( EncodeUtf8 txt
    , DsvError (DuplicateColumn txt) he
    , DsvError (MissingColumn txt) he
    , DsvError RowTooShort re
    )
    => txt -> Lookup he re ByteString

textLookupUtf8' name =
  Lookup $
    View $
      \header ->
        case List.findIndices (== encodeUtf8 name) (Foldable.toList header) of
            []  -> Failure (dsvError (MissingColumn name))
            [i] ->
                Success $
                  View $
                    \row ->
                        case vectorIndexInt row i of
                            Nothing -> Failure (dsvError RowTooShort)
                            Just x -> Success x
            _   -> Failure (dsvError (DuplicateColumn name))

byteStringLookupPosition ::
    forall he re .
    DsvError RowTooShort re
    => Integer
    -> Lookup he re ByteString

byteStringLookupPosition n =
  Lookup $
    View $
      \_header ->
        Success $
          View $
            \row ->
                case vectorIndexInteger row (n - 1) of
                    Nothing -> Failure (dsvError RowTooShort)
                    Just x -> Success x

entireRowLookup :: forall he re . Lookup he re (Vector ByteString)
entireRowLookup = Lookup (constView id)

natLookupUtf8 ::
    forall he re txt .
    ( EncodeUtf8 txt
    , DsvError (InvalidNat txt) re
    , DsvError (DuplicateColumn txt) he
    , DsvError (MissingColumn txt) he
    , DsvError RowTooShort re
    )
    => txt
    -> Lookup he re Natural

natLookupUtf8 name =
  refineLookup
    (textLookupUtf8' name)
    (attoByteStringView (dsvError (InvalidNat name)) Data.Attoparsec.ByteString.Char8.decimal)

attoByteStringView :: e -> Data.Attoparsec.ByteString.Parser a -> View e ByteString a
attoByteStringView e p =
  View $ \bs ->
    case Data.Attoparsec.ByteString.parseOnly (p <* Data.Attoparsec.ByteString.endOfInput) bs of
      Left _ -> Failure e
      Right x -> Success x
