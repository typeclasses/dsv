{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia #-}

module Dsv.ZipViewType
  ( ZipView (..), refineZipView, overHeaderError, mapRowError, mapZipViewError
  ) where

import Dsv.ByteString
import Dsv.Prelude
import Dsv.ViewType
import Dsv.Vector

-- base
import Data.Functor.Compose (Compose (Compose))

newtype ZipView he re a =
  ZipView
    (View he (Vector ByteString)
      (View re (Vector ByteString) a))
  deriving stock Functor
  deriving Applicative
    via
      Compose
        (View he (Vector ByteString))
        (View re (Vector ByteString))

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

mapRowError ::
    (re1 -> re2) ->
    ZipView he re1 a ->
    ZipView he re2 a

mapRowError f (ZipView v) =
    ZipView (fmap (overViewError f) v)

mapZipViewError
    :: (he1 -> he)
    -> (re1 -> re2)
    -> ZipView he1 re1 a
    -> ZipView he re2 a

mapZipViewError f g =
    mapRowError g .
    overHeaderError f
