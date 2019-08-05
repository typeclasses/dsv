{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia #-}

module Dsv.ZipViewType
  ( Lookup (..), refineLookup, overHeaderError, mapRowError, mapLookupError
  ) where

import Dsv.ByteString
import Dsv.Prelude
import Dsv.ViewType
import Dsv.Vector

-- base
import Data.Functor.Compose (Compose (Compose))

newtype Lookup he re a =
  Lookup
    (View he (Vector ByteString)
      (View re (Vector ByteString) a))
  deriving stock Functor
  deriving Applicative
    via
      Compose
        (View he (Vector ByteString))
        (View re (Vector ByteString))

refineLookup ::
    forall he re a b .
    Lookup he re a
    -> View re a b
    -> Lookup he re b

refineLookup (Lookup (View f)) r2 =
  Lookup $
    View $
      fmap (r2 .) . f

overHeaderError ::
    (he1 -> he2) ->
    Lookup he1 re a ->
    Lookup he2 re a

overHeaderError f (Lookup v) =
    Lookup (overViewError f v)

mapRowError ::
    (re1 -> re2) ->
    Lookup he re1 a ->
    Lookup he re2 a

mapRowError f (Lookup v) =
    Lookup (fmap (overViewError f) v)

mapLookupError
    :: (he1 -> he)
    -> (re1 -> re2)
    -> Lookup he1 re1 a
    -> Lookup he re2 a

mapLookupError f g =
    mapRowError g .
    overHeaderError f
