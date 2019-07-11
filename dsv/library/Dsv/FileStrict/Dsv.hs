module Dsv.FileStrict.Dsv
  ( readDsvFileStrictWithZippedHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader
  ) where

import Dsv.Atto
import Dsv.ByteString
import Dsv.Cassava
import Dsv.Delimiter
import Dsv.Fold
import Dsv.Header
import Dsv.IO
import Dsv.Vector

-- pipes
import Pipes

readDsvFileStrictWithoutHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> m (AttoTermination, Vector (Vector ByteString))

readDsvFileStrictWithoutHeader d fp =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h

readDsvFileStrictWithZippedHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> m (AttoTermination, Vector (Vector (ByteString, ByteString)))

readDsvFileStrictWithZippedHeader d fp =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> zipHeaderPipe

readDsvFileStrictIgnoringHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read

    -> m (AttoTermination, Vector (Vector ByteString))

readDsvFileStrictIgnoringHeader d fp =
    liftIO $ runSafeT $
      do
        foldProducerM (foldDropM 1 foldVectorM) $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h
