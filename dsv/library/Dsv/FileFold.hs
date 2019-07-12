module Dsv.FileFold
  ( foldDsvFileWithoutHeader, foldDsvFileWithoutHeaderM
  , foldDsvFileIgnoringHeader, foldDsvFileIgnoringHeaderM
  , foldDsvFileWithZippedHeader, foldDsvFileWithZippedHeaderM
  ) where

import Dsv.AttoTermination
import Dsv.ByteString
import Dsv.DelimiterType
import Dsv.Fold
import Dsv.Header
import Dsv.IO
import Dsv.Parsing
import Dsv.Vector

-- pipes
import Pipes

-- pipes-safe
import qualified Pipes.Safe.Prelude as P

foldDsvFileWithoutHeader
    :: MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldDsvFileWithoutHeader d fp fld =
    liftIO $ runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducer fld (handleDsvRowProducer d h)

foldDsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldDsvFileWithoutHeaderM d fp fld =
    runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducerM fld (handleDsvRowProducer d h)

foldDsvFileIgnoringHeader
    :: MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldDsvFileIgnoringHeader d fp fld =
    foldDsvFileWithoutHeader d fp (foldDrop 1 fld)

foldDsvFileIgnoringHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldDsvFileIgnoringHeaderM d fp fld =
    foldDsvFileWithoutHeaderM d fp (foldDropM 1 fld)

foldDsvFileWithZippedHeader
    :: MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Fold (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldDsvFileWithZippedHeader d fp fld =
    liftIO $ runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducer fld (handleDsvRowProducer d h >-> zipHeaderPipe)

foldDsvFileWithZippedHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> FoldM m (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldDsvFileWithZippedHeaderM d fp fld =
    runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducerM fld (handleDsvRowProducer d h >-> zipHeaderPipe)
