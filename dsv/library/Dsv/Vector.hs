{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.Vector
  ( Vector
  , vectorIndexInt, vectorIndexNat, vectorIndexInteger
  , vectorZip, vectorZipWith
  ) where

import Dsv.Numbers
import Dsv.Prelude

-- base
import Control.Monad ((>=>))

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

vectorIndexInt :: forall a . Vector a -> Int -> Maybe a
vectorIndexInt = (Vector.!?)

vectorIndexNat :: forall a. Vector a -> Natural -> Maybe a
vectorIndexNat xs n = vectorIndexInt xs (fromIntegral n)

vectorIndexInteger ::
    forall a .
    Vector a -> Integer -> Maybe a

vectorIndexInteger xs =
    fromIntegerMaybe >=> vectorIndexInt xs

vectorZip ::
    forall a b .
    Vector a -> Vector b -> Vector (a, b)

vectorZip = Vector.zip

vectorZipWith ::
    forall a b c  .
    (a -> b -> c) -> Vector a -> Vector b -> Vector c

vectorZipWith = Vector.zipWith
