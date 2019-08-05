{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.TextReaderView
  ( textReaderView
  ) where

import Dsv.Prelude
import Dsv.Text
import Dsv.Validation
import Dsv.ViewType

textReaderView :: e -> TextReader a -> View e Text a
textReaderView e r =
  View $ \txt ->
    case (textReadMaybe r txt) of
      Nothing -> Failure e
      Just x -> Success x

