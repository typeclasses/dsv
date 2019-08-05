{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Dsv.ZipViews
  ( byteStringZipView, byteStringZipViewPosition
  , textZipViewUtf8, textZipViewUtf8'
  , entireRowZipView
  ) where

import Dsv.ByteString
import Dsv.Encoding
import Dsv.LookupErrors
import Dsv.Numbers
import Dsv.Position
import Dsv.Prelude
import Dsv.Validation
import Dsv.Vector
import Dsv.Views
import Dsv.ViewType
import Dsv.ZipViewType

-- base
import qualified Data.Foldable as Foldable
import qualified Data.List as List

byteStringZipView
    :: ByteString
    -> ZipView LookupError TooShort ByteString

byteStringZipView name =
  ZipView $
    View $
      \header ->
          case List.findIndices (== name) (Foldable.toList header) of
              []  -> Failure LookupError_Missing
              [i] ->
                  Success $
                    View $
                      \row ->
                          case vectorIndexInt row i of
                              Nothing -> Failure TooShort
                              Just x -> Success x
              _   -> Failure LookupError_Duplicate

textZipViewUtf8 ::
    forall txt e a .
    EncodeUtf8 txt
    => txt
    -> View e ByteString a
    -> ZipView
        (At (ColumnName txt) LookupError)
        (At (ColumnName txt) (IndexError e))
        a

textZipViewUtf8 name fieldView =
  mapZipViewError (At (ColumnName name)) (At (ColumnName name)) $
    ZipView $ View $ \header ->
      case List.findIndices (== encodeUtf8 name) (Foldable.toList header) of
          []  -> Failure LookupError_Missing
          [i] -> withI i
          _   -> Failure LookupError_Duplicate

  where
    withI i = Success $ View $ \row ->
      case vectorIndexInt row i of
        Nothing -> Failure IndexError_TooShort
        Just x -> applyView (overViewError IndexError_FieldError fieldView) x

textZipViewUtf8' ::
    forall txt .
    EncodeUtf8 txt
    => txt
    -> ZipView
        (At (ColumnName txt) LookupError)
        (At (ColumnName txt) TooShort)
        ByteString

textZipViewUtf8' name =
  mapZipViewError (At (ColumnName name)) (At (ColumnName name)) $
    ZipView $ View $ \header ->
      case List.findIndices (== encodeUtf8 name) (Foldable.toList header) of
          []  -> Failure LookupError_Missing
          [i] -> withI i
          _   -> Failure LookupError_Duplicate
  where
    withI i = Success $ View $ \row ->
        case vectorIndexInt row i of
            Nothing -> Failure TooShort
            Just x -> Success x

byteStringZipViewPosition ::
    forall headerError .
    ColumnNumber
    -> ZipView headerError TooShort ByteString

byteStringZipViewPosition (ColumnNumber (Positive n)) =
  ZipView $
    View $
      \_header ->
        Success $
          View $
            \row ->
                case vectorIndexNat row (n - 1) of
                    Nothing -> Failure TooShort
                    Just x -> Success x

entireRowZipView :: forall he re . ZipView he re (Vector ByteString)
entireRowZipView = ZipView (constView id)
