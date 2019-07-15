module Dsv.Pipes
  ( count
  ) where

import Dsv.Numbers

-- pipes
import Pipes

count :: Monad m => Producer a m r -> Producer a m (Natural, r)
count = go 0
  where
    go n p =
      do
        eit <- lift (next p)
        case eit of
            Left r -> return (n, r)
            Right (a, p') -> do { yield a; go (n + 1) p' }
