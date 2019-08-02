{-# LANGUAGE DeriveFunctor, DerivingVia, NoImplicitPrelude #-}

module Dsv.ReadingType
  ( Reading (..)
  ) where

import Dsv.Prelude
import Dsv.Validation

-- base
import Control.Arrow
import Control.Category
import Data.Functor.Compose (Compose (Compose))

newtype Reading e a b =
  Reading
    (a -> Validation e b)
  deriving stock Functor
  deriving Applicative
    via
      Compose
        ((->) a)
        (Validation e)

instance Category (Reading e)
  where
    id = Reading Success

    Reading f . Reading g =
      Reading $ \x ->
        case g x of
          Failure e -> Failure e
          Success y -> f y

instance Arrow (Reading e)
  where
    arr f = Reading (Success . f)

    first (Reading f) =
      Reading $ \(x, y) ->
        case f x of
          Failure e -> Failure e
          Success z -> Success (z, y)

    second (Reading f) =
      Reading $ \(x, y) ->
        case f y of
          Failure e -> Failure e
          Success z -> Success (x, z)
