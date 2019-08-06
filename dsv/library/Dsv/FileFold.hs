{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileFold
  ( foldDsvFileWithoutHeader, foldDsvFileWithoutHeaderM
  , foldDsvFileIgnoringHeader, foldDsvFileIgnoringHeaderM
  , foldDsvFileWithZippedHeader, foldDsvFileWithZippedHeaderM
  ) where

import Dsv.ByteString
import Dsv.DelimiterType
import Dsv.Fold
import Dsv.Header
import Dsv.IO
import Dsv.ParseStop
import Dsv.Parsing
import Dsv.Prelude
import Dsv.Vector

-- pipes
import Pipes

-- pipes-safe
import qualified Pipes.Safe.Prelude as P

foldDsvFileWithoutHeader ::
    forall m result .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldDsvFileWithoutHeader d fp fld =
    liftIO $ runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducer fld (handleDsvRowProducer d h)

foldDsvFileWithoutHeaderM ::
    forall m result .
    (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldDsvFileWithoutHeaderM d fp fld =
    runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducerM fld (handleDsvRowProducer d h)

foldDsvFileIgnoringHeader ::
    forall m result .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldDsvFileIgnoringHeader d fp fld =
    foldDsvFileWithoutHeader d fp (foldDrop 1 fld)

foldDsvFileIgnoringHeaderM ::
    forall m result .
    (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldDsvFileIgnoringHeaderM d fp fld =
    foldDsvFileWithoutHeaderM d fp (foldDropM 1 fld)

foldDsvFileWithZippedHeader ::
    forall m result .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Fold (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldDsvFileWithZippedHeader d fp fld =
    liftIO $ runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducer fld (handleDsvRowProducer d h >-> zipHeaderPipe)

foldDsvFileWithZippedHeaderM ::
    forall m result .
    (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> FoldM m (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldDsvFileWithZippedHeaderM d fp fld =
    runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducerM fld (handleDsvRowProducer d h >-> zipHeaderPipe)
