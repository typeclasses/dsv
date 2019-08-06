{-# LANGUAGE NoImplicitPrelude #-}

module DSV.IO
  ( MonadIO (liftIO), MonadThrow (throwM), MonadCatch, MonadMask
  , IOMode (ReadMode)
  , SafeT, runSafeT
  , withFile
  , Exception (displayException), throw
  , Handle
  ) where

-- base
import Control.Exception (Exception (displayException), throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (IOMode (ReadMode), Handle)

-- pipes-safe
import Pipes.Safe (SafeT, runSafeT, MonadThrow (throwM), MonadCatch, MonadMask)
import Pipes.Safe.Prelude (withFile)
