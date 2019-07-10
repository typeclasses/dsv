module Dsv.Header.Zip
  ( zipNames, zipNames', zipNamesPipe
  ) where

import Dsv.Header.Labeled
import Dsv.Vector

-- pipes
import Pipes
import qualified Pipes.Prelude as P

zipNames :: [Vector a] -> [Vector (Labeled a a)]
zipNames [] = []
zipNames (names : rows) = zipNames' names rows

zipNames' :: Vector a -> [Vector b] -> [Vector (Labeled a b)]
zipNames' names rows = map (vectorZipWith Labeled names) rows

-- | The pipe equivalent of 'zipNames'.
zipNamesPipe :: Monad m => Pipe (Vector a) (Vector (Labeled a a)) m r
zipNamesPipe =
  do
    header <- await
    P.map (vectorZipWith Labeled header)
