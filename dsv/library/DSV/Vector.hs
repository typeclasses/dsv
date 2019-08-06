{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Vector
  ( Vector
  , vectorIndexInt, vectorIndexNat, vectorIndexInteger, nthVectorElement
  , vectorLookup
  , vectorZip, vectorZipWith
  ) where

import DSV.Numbers
import DSV.Prelude

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

{- |

=== Examples

  * @nthVectorElement 0 ["a", "b", "c"] = Nothing@
  * @nthVectorElement 1 ["a", "b", "c"] = Just "a"@
  * @nthVectorElement 2 ["a", "b", "c"] = Just "b"@
  * @nthVectorElement 3 ["a", "b", "c"] = Just "c"@
  * @nthVectorElement 4 ["a", "b", "c"] = Nothing@

-}

nthVectorElement :: forall a . Integer -> Vector a -> Maybe a
nthVectorElement n xs = vectorIndexInteger xs (n - 1)

vectorLookup ::
    forall name value .
    (name -> Bool)
    -> Vector (name, value)
    -> Maybe value

vectorLookup f xs =
  case filter (\(n, _) -> f n) (toList xs) of
    [(_, v)] -> Just v
    _ -> Nothing
