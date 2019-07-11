module Dsv.FileStrict.Dsv
  ( readDsvFileStrictUsingHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader
  ) where

import Dsv.Atto
import Dsv.ByteString
import Dsv.Cassava
import Dsv.Delimiter
import Dsv.Header
import Dsv.IO
import Dsv.Vector

-- pipes
import qualified Pipes.Prelude as P

readDsvFileStrictWithoutHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> m (AttoTermination, [Vector ByteString])

readDsvFileStrictWithoutHeader d fp =
    liftIO $ runSafeT $
      do
        (xs, t) <- P.toListM' $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h
        return (t, xs)

readDsvFileStrictUsingHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> m (AttoTermination, [Vector (ByteString, ByteString)])

readDsvFileStrictUsingHeader d fp =
    fmap (fmap zipNames) (readDsvFileStrictWithoutHeader d fp)

readDsvFileStrictIgnoringHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read

    -> m (AttoTermination, [Vector ByteString])
readDsvFileStrictIgnoringHeader d fp =
    fmap (fmap (drop 1)) (readDsvFileStrictWithoutHeader d fp)
