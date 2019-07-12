module Dsv.FileStrictMap
  ( mapDsvFileStrictWithoutHeader
  , mapDsvFileStrictIgnoringHeader
  , mapDsvFileStrictUsingHeader
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
import qualified Pipes.Prelude as P

mapDsvFileStrictWithoutHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO a)
    -> m (AttoTermination, Vector a)

mapDsvFileStrictWithoutHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> P.mapM (lift . f)

mapDsvFileStrictIgnoringHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO a)
    -> m (AttoTermination, Vector a)

mapDsvFileStrictIgnoringHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM (foldDropM 1 foldVectorM) $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> P.mapM (lift . f)

mapDsvFileStrictUsingHeader
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath   -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO (Vector ByteString -> IO a))
    -> m (AttoTermination, Vector a)

mapDsvFileStrictUsingHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> applyHeaderPipeM (bigLift f)

bigLift
    :: (Vector ByteString ->       IO (Vector ByteString ->       IO a))
    -> (Vector ByteString -> SafeT IO (Vector ByteString -> SafeT IO a))

bigLift = fmap liftIO . ((fmap . fmap . fmap) liftIO)
