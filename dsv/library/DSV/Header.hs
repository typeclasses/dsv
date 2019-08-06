{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Header
  ( zipHeader, zipHeader', zipHeaderPipe
  , zipHeaderWith, zipHeaderWith', zipHeaderWithPipe
  , applyHeaderPipe, applyHeaderPipeM
  ) where

import DSV.Prelude
import DSV.Vector

-- pipes
import Pipes
import qualified Pipes.Prelude as P

zipHeader :: forall a . [Vector a] -> [Vector (a, a)]
zipHeader [] = []
zipHeader (names : rows) = zipHeader' names rows

zipHeader' :: forall a b . Vector a -> [Vector b] -> [Vector (a, b)]
zipHeader' names rows = map (vectorZip names) rows

zipHeaderWith :: forall a b . (a -> a -> b) -> [Vector a] -> [Vector b]
zipHeaderWith _ [] = []
zipHeaderWith f (names : rows) = zipHeaderWith' f names rows

zipHeaderWith' :: forall a b c . (a -> b -> c)
    -> Vector a -> [Vector b] -> [Vector c]
zipHeaderWith' f names rows = map (vectorZipWith f names) rows

applyHeaderPipe ::
    forall a b m r .
    Monad m
    => (a -> a -> b)
    -> Pipe a b m r

applyHeaderPipe f =
  do
    header <- await
    P.map (f header)

applyHeaderPipeM ::
    forall a b m r .
    Monad m
    => (a -> m (a -> m b))
    -> Pipe a b m r

applyHeaderPipeM f =
  do
    header <- await
    applyHeader <- lift (f header)
    P.mapM applyHeader

-- | The pipe equivalent of 'zipHeader'.
zipHeaderPipe ::
    forall a m r .
    Monad m
    => Pipe (Vector a) (Vector (a, a)) m r

zipHeaderPipe = applyHeaderPipe vectorZip

-- | The pipe equivalent of 'zipHeaderWith'.
zipHeaderWithPipe ::
    forall a b m r .
    Monad m
    => (a -> a -> b)
    -> Pipe (Vector a) (Vector b) m r

zipHeaderWithPipe f = applyHeaderPipe (vectorZipWith f)
