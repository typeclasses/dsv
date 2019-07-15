{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileStrictLookup
  ( lookupDsvFileStrict
  , lookupDsvFileStrictIgnoringAllErrors
  , lookupDsvFileStrictThrowFirstError
  ) where

import Dsv.DelimiterType
import Dsv.Fold
import Dsv.IO
import Dsv.Lens
import Dsv.LookupPipe
import Dsv.LookupType
import Dsv.ParseLookupTermination
import Dsv.Parsing
import Dsv.Validation
import Dsv.Vector

-- pipes
import Pipes

lookupDsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Lookup headerError rowError row
        -- ^ How to interpret the rows
    -> m (ParseLookupTermination headerError, Vector (Validation rowError row))

lookupDsvFileStrict d fp lu =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                fmap (review parseLookupTerminationEitherIso) $
                    fmap Left (handleDsvRowProducer d h) >->
                    fmap Right (lookupPipe lu)

lookupDsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Lookup headerError rowError row
        -- ^ How to interpret the rows
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
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> Lookup headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

lookupDsvFileStrictThrowFirstError d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                lookupPipeThrowFirstError lu
