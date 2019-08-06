{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.FileStrictRead
  ( readDsvFileStrictWithZippedHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader
  ) where

import DSV.ByteString
import DSV.DelimiterType
import DSV.Fold
import DSV.Header
import DSV.IO
import DSV.ParseStop
import DSV.Parsing
import DSV.Prelude
import DSV.Vector

-- pipes
import Pipes

readDsvFileStrictWithoutHeader ::
    forall m .
    MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> m (ParseStop, Vector (Vector ByteString))

readDsvFileStrictWithoutHeader d fp =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h

readDsvFileStrictWithZippedHeader ::
    forall m .
    MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> m (ParseStop, Vector (Vector (ByteString, ByteString)))

readDsvFileStrictWithZippedHeader d fp =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> zipHeaderPipe

readDsvFileStrictIgnoringHeader ::
    forall m .
    MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read

    -> m (ParseStop, Vector (Vector ByteString))

readDsvFileStrictIgnoringHeader d fp =
    liftIO $ runSafeT $
      do
        foldProducerM (foldDropM 1 foldVectorM) $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h
