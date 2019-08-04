{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.Views
  ( constView
  , natTextView
  ) where

import Dsv.Numbers
import Dsv.Prelude
import Dsv.Text
import Dsv.Validation
import Dsv.ViewType

constView :: forall e a b. b -> View e a b
constView x = View (\_ -> Success x)

natTextView :: View () Text Natural
natTextView =
  View $ \txt ->
    case (textReadMaybe textReadDecimal txt) of
      Nothing -> Failure ()
      Just x -> Success x
