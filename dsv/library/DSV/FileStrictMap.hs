{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.FileStrictMap
  ( mapDsvFileStrictWithoutHeader
  , mapDsvFileStrictIgnoringHeader
  , mapDsvFileStrictUsingHeader
  ) where

import DSV.ByteString
import DSV.DelimiterType
import DSV.Fold
import DSV.Header
import DSV.IO
import DSV.ParseStop
import DSV.Parsing
import DSV.Pipes
import DSV.Prelude
import DSV.Vector

-- pipes
import qualified Pipes.Prelude as P

mapDsvFileStrictWithoutHeader ::
    forall m row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the DSV file
    -> m (ParseStop, Vector row)

mapDsvFileStrictWithoutHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> P.mapM (lift . f)

mapDsvFileStrictIgnoringHeader ::
    forall m row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the DSV file
    -> m (ParseStop, Vector row)

mapDsvFileStrictIgnoringHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM (foldDropM 1 foldVectorM) $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> P.mapM (lift . f)

mapDsvFileStrictUsingHeader ::
    forall m row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> (Vector ByteString -> IO (Vector ByteString -> IO row))
        -- ^ Function which interprets the header (the first @Vector ByteString@) and returns a conversion function (@Vector ByteString -> IO row@) by which you specify how to interpret one row of bytes from the DSV file
    -> m (ParseStop, Vector row)

mapDsvFileStrictUsingHeader d fp f =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                handleDsvRowProducer d h >-> applyHeaderPipeM (bigLift f)

bigLift ::
       (Vector ByteString ->       IO (Vector ByteString ->       IO a))
    -> (Vector ByteString -> SafeT IO (Vector ByteString -> SafeT IO a))

bigLift = fmap liftIO . ((fmap . fmap . fmap) liftIO)
