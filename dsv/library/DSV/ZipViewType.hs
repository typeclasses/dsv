{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia, StandaloneDeriving #-}

module DSV.ZipViewType
  ( ZipView (..), refineZipView
  , overHeaderError, overRowError, overZipViewError
  ) where

import DSV.ByteString
import DSV.Prelude
import DSV.ViewType
import DSV.Vector

-- base
import Data.Functor.Compose (Compose (Compose))

newtype ZipView he re a =
  ZipView
    (View he (Vector ByteString)
      (View re (Vector ByteString) a))
  deriving stock Functor

deriving via
  Compose
    (View headerError (Vector ByteString))
    (View rowError (Vector ByteString))
  instance
    (Semigroup headerError, Semigroup rowError) =>
    Applicative (ZipView headerError rowError)

refineZipView ::
    forall he re a b .
    ZipView he re a
    -> View re a b
    -> ZipView he re b

refineZipView (ZipView (View f)) r2 =
  ZipView $
    View $
      fmap (r2 .) . f

overHeaderError ::
    (he1 -> he2) ->
    ZipView he1 re a ->
    ZipView he2 re a

overHeaderError f (ZipView v) =
    ZipView (overViewError f v)

overRowError ::
    (re1 -> re2) ->
    ZipView he re1 a ->
    ZipView he re2 a

overRowError f (ZipView v) =
    ZipView (fmap (overViewError f) v)

overZipViewError
    :: (he1 -> he)
    -> (re1 -> re2)
    -> ZipView he1 re1 a
    -> ZipView he re2 a

overZipViewError f g =
    overRowError g .
    overHeaderError f
