{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia, StandaloneDeriving #-}

module DSV.ViewType
  ( View (..), overViewError, applyView, constView, viewMaybe
  ) where

import DSV.Prelude
import DSV.Validation

-- base
import Control.Arrow
import Control.Category

newtype View e a b = View (a -> Validation e b)
    deriving stock Functor

deriving via Compose ((->) a) (Validation e)
    instance Semigroup e => Applicative (View e a)

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

applyView :: forall e a b .
    View e a b -> a -> Validation e b

applyView (View f) x = f x

viewMaybe :: forall e a b .
    View e a b -> a -> Maybe b

viewMaybe v x =
    case applyView v x of
        Success y -> Just y
        Failure _ -> Nothing

constView :: forall e a b .
    b -> View e a b

constView x = View (\_ -> Success x)

overViewError :: forall e1 e2 a b .
    (e1 -> e2) -> View e1 a b -> View e2 a b

overViewError f (View v) =
  View $ \x ->
    case v x of
      Failure e -> Failure (f e)
      Success s -> Success s
