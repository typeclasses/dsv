{-# LANGUAGE DeriveFunctor, DerivingVia, NoImplicitPrelude #-}

module Dsv.LookupType
  ( Lookup (..)
  ) where

import Dsv.ByteString
import Dsv.Prelude
import Dsv.ReadingType
import Dsv.Vector

-- base
import Data.Functor.Compose (Compose (Compose))

newtype Lookup he re a =
  Lookup
    (Reading he (Vector ByteString)
      (Reading re (Vector ByteString) a))
  deriving stock Functor
  deriving Applicative
    via
      Compose
        (Reading he (Vector ByteString))
        (Reading re (Vector ByteString))
