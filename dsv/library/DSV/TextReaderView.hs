{-# LANGUAGE NoImplicitPrelude #-}

module DSV.TextReaderView
  ( textReaderView
  , textReaderView_
  ) where

import DSV.Prelude
import DSV.Text
import DSV.Validation
import DSV.ViewType

textReaderView :: e -> TextReader a -> View e Text a
textReaderView e r =
  View $ \txt ->
    case (textReadMaybe r txt) of
      Nothing -> Failure e
      Just x -> Success x

textReaderView_ :: TextReader a -> View () Text a
textReaderView_ r =
  View $ \txt ->
    case (textReadMaybe r txt) of
      Nothing -> Failure ()
      Just x -> Success x
