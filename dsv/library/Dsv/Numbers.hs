{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, LambdaCase, NoImplicitPrelude, ScopedTypeVariables, TypeApplications #-}

module Dsv.Numbers
  ( fromIntegerMaybe
  , Natural
  , ArithException (..)
  , Positive (..), natPositive, positiveNat
  ) where

import Dsv.IO
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

newtype Positive = Positive Natural
  deriving newtype (Eq, Ord, Show)

natPositive :: Natural -> Positive
natPositive =
  \case
    0 -> throw Underflow
    n -> Positive n

positiveNat :: Positive -> Natural
positiveNat (Positive n) = n

instance Num Positive
  where
    fromInteger n | n < 1 = throw Underflow
                  | True  = Positive (fromInteger n)

    Positive x + Positive y = Positive    (x + y)
    Positive x - Positive y = natPositive (x - y)
    Positive x * Positive y = Positive    (x * y)

    negate _ = throw Underflow
    abs = id
    signum _ = 1
