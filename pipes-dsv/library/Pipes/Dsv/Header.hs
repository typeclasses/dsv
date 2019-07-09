module Pipes.Dsv.Header
  ( Labeled (..)
  , zipNames, zipNames'
  , zipNamesPipe
  ) where

import Pipes.Dsv.Vector

-- base
import Data.Bifunctor

-- pipes
import Pipes
import qualified Pipes.Prelude as P

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

-- | The pipe equivalent of 'zipNames'.
zipNamesPipe :: Monad m => Pipe (Vector a) (Vector (Labeled a a)) m ()
zipNamesPipe =
  do
    header <- await
    P.map (vectorZipWith Labeled header)
