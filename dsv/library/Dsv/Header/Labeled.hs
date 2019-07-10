module Dsv.Header.Labeled
  ( Labeled (..)
  ) where

-- base
import Data.Bifunctor

data Labeled name a = Labeled { name :: name, value :: a }
    deriving (Eq, Show)

instance Functor (Labeled name) where
    fmap f (Labeled n x) = Labeled n (f x)

instance Bifunctor Labeled where
    bimap f g (Labeled n x) = Labeled (f n) (g x)
