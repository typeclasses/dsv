module Dsv.Header.Zip
  ( zipNames, zipNames', zipNamesPipe
  ) where

import Dsv.Vector

-- pipes
import Pipes
import qualified Pipes.Prelude as P

zipNames :: [Vector a] -> [Vector (a, a)]
zipNames [] = []
zipNames (names : rows) = zipNames' names rows

zipNames' :: Vector a -> [Vector b] -> [Vector (a, b)]
zipNames' names rows = map (vectorZip names) rows

-- | The pipe equivalent of 'zipNames'.
zipNamesPipe :: Monad m => Pipe (Vector a) (Vector (a, a)) m r
zipNamesPipe =
  do
    header <- await
    P.map (vectorZip header)
