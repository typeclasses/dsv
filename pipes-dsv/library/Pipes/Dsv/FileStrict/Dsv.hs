module Pipes.Dsv.FileStrict.Dsv
  ( readDsvFileStrictUsingHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Cassava
import Pipes.Dsv.Delimiter
import Pipes.Dsv.Header
import Pipes.Dsv.Vector

-- base
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.IO (IOMode (ReadMode))

-- pipes
import qualified Pipes.Prelude as P

-- pipes-safe
import qualified Pipes.Safe.Prelude as P
import Pipes.Safe (runSafeT)

readDsvFileStrictWithoutHeader :: MonadIO m => Delimiter -> FilePath -> m (AttoTermination, [Vector ByteString])
readDsvFileStrictWithoutHeader d fp =
    liftIO $ runSafeT $
      do
        (xs, t) <- P.toListM' $
            P.withFile fp ReadMode (handleAttoProducer (dsvRowAtto d))
        return (t, xs)

readDsvFileStrictUsingHeader :: MonadIO m => Delimiter -> FilePath -> m (AttoTermination, [Vector (Labeled ByteString ByteString)])
readDsvFileStrictUsingHeader d fp =
    fmap (fmap zipNames) (readDsvFileStrictWithoutHeader d fp)

readDsvFileStrictIgnoringHeader :: MonadIO m => Delimiter -> FilePath -> m (AttoTermination, [Vector ByteString])
readDsvFileStrictIgnoringHeader d fp =
    fmap (fmap (drop 1)) (readDsvFileStrictWithoutHeader d fp)
