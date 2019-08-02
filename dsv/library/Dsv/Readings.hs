{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.Readings
  ( constReading
  , natTextReading
  ) where

import Dsv.Numbers
import Dsv.Prelude
import Dsv.ReadingType
import Dsv.Text
import Dsv.Validation

constReading :: forall e a b. b -> Reading e a b
constReading x = Reading (\_ -> Success x)

natTextReading :: Reading () Text Natural
natTextReading =
  Reading $ \txt ->
    case (textReadMaybe textReadDecimal txt) of
      Nothing -> Failure ()
      Just x -> Success x
