module Dsv.Header.Zip
  ( zipHeader, zipHeader', zipHeaderPipe
  ) where

import Dsv.Vector

-- pipes
import Pipes
import qualified Pipes.Prelude as P

zipHeader :: [Vector a] -> [Vector (a, a)]
zipHeader [] = []
zipHeader (names : rows) = zipHeader' names rows

zipHeader' :: Vector a -> [Vector b] -> [Vector (a, b)]
zipHeader' names rows = map (vectorZip names) rows

-- | The pipe equivalent of 'zipHeader'.
zipHeaderPipe :: Monad m => Pipe (Vector a) (Vector (a, a)) m r
zipHeaderPipe =
  do
    header <- await
    P.map (vectorZip header)
