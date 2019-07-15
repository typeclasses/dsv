{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileStrictLookup
  ( lookupDsvFileStrict
  , lookupDsvFileStrictIgnoringAllErrors
  , lookupDsvFileStrictThrowFirstError
  ) where

import Dsv.AttoLookupTermination
import Dsv.DelimiterType
import Dsv.Fold
import Dsv.IO
import Dsv.Lens
import Dsv.LookupPipe
import Dsv.LookupType
import Dsv.Parsing
import Dsv.Validation
import Dsv.Vector

-- pipes
import Pipes

lookupDsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
    -> FilePath
    -> Lookup headerError rowError row
    -> m (AttoLookupTermination headerError, Vector (Validation rowError row))

lookupDsvFileStrict d fp lu =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                fmap (review attoLookupTerminationEitherIso) $
                    fmap Left (handleDsvRowProducer d h) >->
                    fmap Right (lookupPipe lu)

lookupDsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
    -> FilePath
    -> Lookup headerError rowError row
    -> m (Vector row)

lookupDsvFileStrictIgnoringAllErrors d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                lookupPipeIgnoringAllErrors lu

lookupDsvFileStrictThrowFirstError ::
    forall m headerError rowError row .
    (MonadIO m, Exception headerError, Exception rowError)
    => Delimiter
    -> FilePath
    -> Lookup headerError rowError row
    -> m (Vector row)

lookupDsvFileStrictThrowFirstError d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                lookupPipeThrowFirstError lu
