module Dsv.Vector
  ( Vector, vectorZip, vectorZipWith
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

vectorZip :: Vector a -> Vector b -> Vector (a, b)
vectorZip = Vector.zip

vectorZipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
vectorZipWith = Vector.zipWith
