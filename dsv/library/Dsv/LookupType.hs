{-# LANGUAGE DeriveFunctor, DerivingVia, NoImplicitPrelude #-}

module Dsv.LookupType
  ( Lookup (..)
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
