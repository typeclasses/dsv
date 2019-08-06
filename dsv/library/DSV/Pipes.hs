{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Pipes
  ( count
  ) where

import DSV.Numbers
import DSV.Prelude

-- pipes
import Pipes

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
