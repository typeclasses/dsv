{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia #-}

module Dsv.ViewType
  ( View (..), overViewError, applyView
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

applyView :: View e a b -> a -> Validation e b
applyView (View f) x = f x

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

overViewError :: (e1 -> e2) -> View e1 a b -> View e2 a b
overViewError f (View v) =
  View $ \x ->
    case v x of
      Failure e -> Failure (f e)
      Success s -> Success s
