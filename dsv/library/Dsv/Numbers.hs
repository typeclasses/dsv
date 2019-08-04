{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, TypeApplications #-}

module Dsv.Numbers
  ( fromIntegerMaybe
  , Natural
  , ArithException (..)
  ) where

import Dsv.Prelude

-- base
import Control.Exception (ArithException (..))
import Numeric.Natural (Natural)

fromIntegerMaybe ::
    forall n .
    (Bounded n, Integral n)
    => Integer -> Maybe n

fromIntegerMaybe i
    | i < toInteger (minBound @n) = Nothing
    | i > toInteger (maxBound @n) = Nothing
    | otherwise                   = Just (fromInteger @n i)
