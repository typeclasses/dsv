{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia, StandaloneDeriving #-}

module DSV.ViewType
  ( View (..), overViewError, inputAsViewError, applyView, viewOrThrow, viewOrThrowInput, constView, maybeView, viewMaybe, viewOr, viewOr'
  , discardViewError, (>>>-), (<<<-)
  ) where

import DSV.IO
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

viewOrThrow :: forall m e a b.
    (Exception e, MonadThrow m) =>
    View e a b -> a -> m b

viewOrThrow (View f) x =
    case f x of
        Failure e -> throwM e
        Success y -> pure y

viewMaybe :: forall e a b .
    View e a b -> a -> Maybe b

viewMaybe v x =
    case applyView v x of
        Success y -> Just y
        Failure _ -> Nothing

viewOr :: forall e a b .
    b -> View e a b -> a -> b

viewOr z v x =
    case applyView v x of
        Success y -> y
        Failure _ -> z

viewOr' :: forall m e a b .
    Applicative m =>
    (a -> e -> m b)
    -> View e a b
    -> a
    -> m b

viewOr' f v a =
    case applyView v a of
        Failure e -> f a e
        Success b -> pure b

viewOrThrowInput :: forall m ex e a b .
    (Exception ex, MonadThrow m) =>
    (a -> ex)
    -> View e a b
    -> a
    -> m b

viewOrThrowInput f v a =
    case applyView v a of
        Failure _ -> throwM (f a)
        Success b -> pure b

constView :: forall e a b .
    b -> View e a b

constView x = View (\_ -> Success x)

maybeView :: forall a b.
    (a -> Maybe b)
    -> View () a b

maybeView f = View (maybe (Failure ()) Success . f)

overViewError :: forall e1 e2 a b .
    (e1 -> e2) -> View e1 a b -> View e2 a b

overViewError f (View v) =
  View $ \x ->
    case v x of
      Failure e -> Failure (f e)
      Success s -> Success s

inputAsViewError :: forall e a b. View e a b -> View a a b
inputAsViewError (View v) =
    View $ \x ->
      case v x of
        Failure _ -> Failure x
        Success s -> Success s

discardViewError :: View e a b -> View () a b

discardViewError = overViewError (\_ -> ())

(<<<-) :: View e1 b c -> View e2 a b -> View () a c
f <<<- g = discardViewError f <<< discardViewError g

(>>>-) :: View e2 a b -> View e1 b c -> View () a c
f >>>- g = discardViewError f >>> discardViewError g
