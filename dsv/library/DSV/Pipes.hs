{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Pipes
  ( Pipe, Producer, Consumer, Effect
  , yield, await, (>->), lift, runEffect
  , count
  ) where

import DSV.Numbers
import DSV.Prelude

-- pipes
import Pipes (Proxy, lift, next, await, yield, runEffect)
import qualified Pipes

-- | Use @>->@ to build an 'Effect': a pipeline consisting of a 'Producer' at the beginning, any number of 'Pipe's in the middle, and a 'Consumer' at the end.
(>->) :: Monad m => Proxy a' a () b m r -> Proxy () b c' c m r -> Proxy a' a c' c m r
(>->) = (Pipes.>->)

type Producer b m r = Pipes.Producer b m r

type Pipe a b m r = Pipes.Pipe a b m r

type Consumer a m r = Pipes.Consumer a m r

type Effect m r = Pipes.Effect m r

count ::
    forall a m r .
    Monad m
    => Producer a m r
    -> Producer a m (Natural, r)

count = go 0
  where
    go n p =
        lift (next p) >>=
        \case
            Left r -> return (n, r)
            Right (a, p') -> yield a *> go (n + 1) p'
