module Pipes.Dsv.IO
  ( MonadIO (liftIO), MonadCatch, MonadMask
  , IOMode (ReadMode)
  , SafeT, runSafeT
  , withFile
  , Exception
  , Handle
  ) where

-- base
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (IOMode (ReadMode), Handle)

-- pipes-safe
import Pipes.Safe (SafeT, runSafeT, MonadCatch, MonadMask)
import Pipes.Safe.Prelude (withFile)
