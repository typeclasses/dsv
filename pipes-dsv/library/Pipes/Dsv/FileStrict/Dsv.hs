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
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

-- pipes
import qualified Pipes.Prelude as P

readDsvFileStrictWithoutHeader :: MonadIO m => Delimiter -> FilePath -> m (AttoTermination, [Vector ByteString])
readDsvFileStrictWithoutHeader d fp =
    liftIO $ runSafeT $
      do
        (xs, t) <- P.toListM' $
            withFile fp ReadMode (handleAttoProducer (dsvRowAtto d))
        return (t, xs)

readDsvFileStrictUsingHeader :: MonadIO m => Delimiter -> FilePath -> m (AttoTermination, [Vector (Labeled ByteString ByteString)])
readDsvFileStrictUsingHeader d fp =
    fmap (fmap zipNames) (readDsvFileStrictWithoutHeader d fp)

readDsvFileStrictIgnoringHeader :: MonadIO m => Delimiter -> FilePath -> m (AttoTermination, [Vector ByteString])
readDsvFileStrictIgnoringHeader d fp =
    fmap (fmap (drop 1)) (readDsvFileStrictWithoutHeader d fp)
