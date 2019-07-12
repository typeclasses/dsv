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
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the DSV file
    -> m (AttoTermination, Vector row)

mapDsvFileStrictWithoutHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> P.mapM (lift . f)

mapDsvFileStrictIgnoringHeader
    :: MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the DSV file
    -> m (AttoTermination, Vector row)

mapDsvFileStrictIgnoringHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM (foldDropM 1 foldVectorM) $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> P.mapM (lift . f)

mapDsvFileStrictUsingHeader
    :: MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO (Vector ByteString -> IO row))
        -- ^ Function which interprets the header (the first @Vector ByteString@) and returns a conversion function (@Vector ByteString -> IO row@) by which you specify how to interpret one row of bytes from the CSV file
    -> m (AttoTermination, Vector row)

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
