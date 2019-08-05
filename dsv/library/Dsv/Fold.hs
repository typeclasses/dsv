{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.Fold
  ( Fold, FoldM
  , foldDrop, foldDropM
  , foldProducer, foldProducerM
  , foldVectorM
  ) where

import Dsv.Numbers
import Dsv.Prelude

-- foldl
import qualified Control.Foldl as L
import Control.Foldl (Fold (Fold), FoldM (FoldM))

-- pipes
import Pipes (Producer)
import qualified Pipes.Prelude as P

foldDrop ::
    forall a b .
    Natural -> Fold a b -> Fold a b

foldDrop n (Fold step begin done) =
    Fold step' begin' done'
  where
    begin'          = (n, begin)
    step' (0,  s) x = (0, step s x)
    step' (n', s) _ = (n' - 1, s)
    done' (_,  s)   = done s

foldDropM ::
    forall m a b .
    Monad m
    => Natural -> FoldM m a b -> FoldM m a b

foldDropM n (FoldM step begin done) =
    FoldM step' begin' done'
  where
    begin'          = fmap (\s  -> (n, s))  begin
    step' (0,  s) x = fmap (\s' -> (0, s')) (step s x)
    step' (n', s) _ = return (n' - 1, s)
    done' (_,  s)   = done s

foldProducer ::
    forall a b m r.
    Monad m
    => Fold a b -> Producer a m r -> m (r, b)

foldProducer fld p =
  do
    (x, r) <- L.purely P.fold' fld p
    return (r, x)

foldProducerM ::
    forall a b m r .
    Monad m
    => FoldM m a b -> Producer a m r -> m (r, b)

foldProducerM fld p =
  do
    (x, r) <- L.impurely P.foldM' fld p
    return (r, x)

foldVectorM ::
    forall v m a .
    (L.PrimMonad m, L.Vector v a)
    => FoldM m a (v a)

foldVectorM = L.vectorM
