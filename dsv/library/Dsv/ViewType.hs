{-# LANGUAGE DeriveFunctor, DerivingVia, NoImplicitPrelude #-}

module Dsv.ViewType
  ( View (..)
  ) where

import Dsv.Prelude
import Dsv.Validation

-- base
import Control.Arrow
import Control.Category

newtype View e a b =
  View
    (a -> Validation e b)
  deriving stock Functor
  deriving Applicative
    via
      Compose
        ((->) a)
        (Validation e)

instance Category (View e)
  where
    id = View Success

    View f . View g =
      View $ \x ->
        case g x of
          Failure e -> Failure e
          Success y -> f y

instance Arrow (View e)
  where
    arr f = View (Success . f)

    first (View f) =
      View $ \(x, y) ->
        case f x of
          Failure e -> Failure e
          Success z -> Success (z, y)

    second (View f) =
      View $ \(x, y) ->
        case f y of
          Failure e -> Failure e
          Success z -> Success (x, z)
