module DSV.RequireCompleteParse
  ( requireCompleteParse
  ) where

import DSV.IO
import DSV.ParseError
import DSV.ParseStop

requireCompleteParse :: MonadThrow m => (ParseStop, a) -> m a
requireCompleteParse (s, x) =
    case s of
        ParseComplete -> pure x
        ParseIncomplete -> throwM ParseError
