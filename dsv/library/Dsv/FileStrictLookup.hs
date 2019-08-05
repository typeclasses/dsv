{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Dsv.FileStrictLookup
  ( lookupDsvFileStrict
  , lookupDsvFileStrictIgnoringAllErrors
  , lookupDsvFileStrictThrowFirstError
  ) where

import Dsv.DelimiterType
import Dsv.Fold
import Dsv.IO
import Dsv.ZipViewPipe
import Dsv.ParseTermination
import Dsv.ZipViewStop
import Dsv.Parsing
import Dsv.Pipes
import Dsv.Prelude
import Dsv.Validation
import Dsv.Vector
import Dsv.ZipViewType

-- pipes
import Pipes

lookupDsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (ZipViewStop headerError, Vector (Validation rowError row))

lookupDsvFileStrict d fp lu =
    liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                readRows h >-> interpretRows

  where
    readRows h =
      fmap
        (
          \case
            (_, ParseIncomplete) -> ZipViewParseError
            (0, ParseComplete) -> ZipViewEmpty
            (_, ParseComplete) -> ZipViewComplete
        )
        (count (handleDsvRowProducer d h))

    interpretRows =
      fmap ZipViewHeaderError (zipViewPipe lu)

lookupDsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

lookupDsvFileStrictIgnoringAllErrors d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                zipViewPipeIgnoringAllErrors lu

lookupDsvFileStrictThrowFirstError ::
    forall m headerError rowError row .
    ( MonadIO m
    , Exception headerError
    , Show rowError, Typeable rowError
    )
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

lookupDsvFileStrictThrowFirstError d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                zipViewPipeThrowFirstError lu
