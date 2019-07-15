module Dsv.Vector
  ( Vector, vectorIndexInt, vectorIndexInteger, vectorZip, vectorZipWith
  ) where

import Dsv.Numbers

-- base
import Control.Monad ((>=>))

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

vectorIndexInt :: Vector a -> Int -> Maybe a
vectorIndexInt = (Vector.!?)

vectorIndexInteger :: Vector a -> Integer -> Maybe a
vectorIndexInteger xs = fromIntegerMaybe >=> vectorIndexInt xs

vectorZip :: Vector a -> Vector b -> Vector (a, b)
vectorZip = Vector.zip

vectorZipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
vectorZipWith = Vector.zipWith
