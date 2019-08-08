{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module DSV.ZipViews
  ( byteStringZipView, byteStringZipViewPosition
  , textZipViewUtf8, textZipViewUtf8'
  , entireRowZipView
  ) where

import DSV.ByteString
import DSV.IndexError
import DSV.LookupError
import DSV.Numbers
import DSV.Position
import DSV.Prelude
import DSV.Text
import DSV.UTF8
import DSV.Validation
import DSV.Vector
import DSV.ViewType
import DSV.ZipViewType

-- base
import qualified Data.List as List

byteStringZipView
    :: ByteString
    -> ZipView LookupError TooShort ByteString

byteStringZipView name =
    (ZipView . View) $ \header ->
    case List.findIndices (== name) (toList header) of
        []  -> Failure LookupError_Missing
        [i] -> withI i
        _   -> Failure LookupError_Duplicate
  where
    withI i =
        (Success . View) $ \row ->
        case vectorIndexInt row i of
            Nothing -> Failure TooShort
            Just x  -> Success x

textZipViewUtf8 ::
    forall e a .
    Text
    -> View e ByteString a
    -> ZipView
        (At (ColumnName Text) LookupError)
        (At (ColumnName Text) (IndexError e))
        a

textZipViewUtf8 name fieldView =
    (overZipViewError at at . ZipView . View) $ \header ->
    case List.findIndices (== encodeUtf8 name) (toList header) of
        []  -> Failure LookupError_Missing
        [i] -> withI i
        _   -> Failure LookupError_Duplicate

  where
    at = At (ColumnName name)
    withI i =
        (Success . View) $ \row ->
        case vectorIndexInt row i of
            Nothing -> Failure IndexError_TooShort
            Just x -> applyView (overViewError IndexError_FieldError fieldView) x

textZipViewUtf8' ::
    Text
    -> ZipView
        (At (ColumnName Text) LookupError)
        (At (ColumnName Text) TooShort)
        ByteString

textZipViewUtf8' name =
    (overZipViewError at at . ZipView . View) $ \header ->
    case List.findIndices (== encodeUtf8 name) (toList header) of
        []  -> Failure LookupError_Missing
        [i] -> withI i
        _   -> Failure LookupError_Duplicate
  where
    at = At (ColumnName name)
    withI i =
        (Success . View) $ \row ->
        case vectorIndexInt row i of
            Nothing -> Failure TooShort
            Just x  -> Success x

byteStringZipViewPosition ::
    forall headerError .
    ColumnNumber
    -> ZipView headerError TooShort ByteString

byteStringZipViewPosition (ColumnNumber (Positive n)) =
    (ZipView . View) $ \_header ->
    (Success . View) $ \row     ->
    case vectorIndexNat row (n - 1) of
        Nothing -> Failure TooShort
        Just x  -> Success x

entireRowZipView :: forall he re . ZipView he re (Vector ByteString)
entireRowZipView = ZipView (constView id)
