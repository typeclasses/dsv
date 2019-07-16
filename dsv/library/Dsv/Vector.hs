{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.Vector
  ( Vector, vectorIndexInt, vectorIndexInteger, vectorZip, vectorZipWith
  ) where

import Dsv.Numbers

-- base
import Control.Monad ((>=>))

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

vectorIndexInt :: forall a . Vector a -> Int -> Maybe a
vectorIndexInt = (Vector.!?)

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
