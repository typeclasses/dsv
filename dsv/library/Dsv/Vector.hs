module Dsv.Vector
  ( Vector, vectorZipWith
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

vectorZipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
vectorZipWith = Vector.zipWith
