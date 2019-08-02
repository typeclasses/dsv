{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.Readings
  ( constReading
  --, natTextReading
  ) where

--import Dsv.Numbers
import Dsv.ReadingType
--import Dsv.Text
import Dsv.Validation

constReading :: forall e a b. b -> Reading e a b
constReading x = Reading (\_ -> Success x)

{- todo
natTextReading :: Reading re Text Natural
natTextReading = _
-}
