module Pipes.Dsv.Header
  ( Labeled (..)
  , zipNames, zipNames'
  ) where

import Pipes.Dsv.Vector

-- base
import Data.Bifunctor

data Labeled name a = Labeled { name :: name, value :: a }
    deriving (Eq, Show)

instance Functor (Labeled name) where
    fmap f (Labeled n x) = Labeled n (f x)

instance Bifunctor Labeled where
    bimap f g (Labeled n x) = Labeled (f n) (g x)

zipNames :: [Vector a] -> [Vector (Labeled a a)]
zipNames [] = []
zipNames (names : rows) = zipNames' names rows

zipNames' :: Vector a -> [Vector b] -> [Vector (Labeled a b)]
zipNames' names rows = map (vectorZipWith Labeled names) rows
