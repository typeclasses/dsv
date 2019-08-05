{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.Views
  ( constView
  ) where

import Dsv.Validation
import Dsv.ViewType

constView :: forall e a b. b -> View e a b
constView x = View (\_ -> Success x)
