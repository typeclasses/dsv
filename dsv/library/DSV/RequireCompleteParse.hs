module DSV.RequireCompleteParse
  ( requireCompleteParse
  , completely
  ) where

import DSV.IO
import DSV.ParseError
import DSV.ParseStop

requireCompleteParse :: MonadThrow m => (ParseStop, a) -> m a
requireCompleteParse (s, x) =
    case s of
        ParseComplete -> pure x
        ParseIncomplete -> throwM ParseError

completely :: MonadThrow m => m (ParseStop, a) -> m a
completely x = x >>= requireCompleteParse
