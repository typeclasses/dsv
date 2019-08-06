{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module DSV.FileStrictZipView
  ( zipViewDsvFileStrict
  , zipViewDsvFileStrictIgnoringAllErrors
  , zipViewDsvFileStrictThrowFirstError
  ) where

import DSV.DelimiterType
import DSV.Fold
import DSV.IO
import DSV.ParseStop
import DSV.Parsing
import DSV.Pipes
import DSV.Prelude
import DSV.Validation
import DSV.Vector
import DSV.ZipViewPipe
import DSV.ZipViewStop
import DSV.ZipViewType

zipViewDsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (ZipViewStop headerError, Vector (Validation rowError row))

zipViewDsvFileStrict d fp lu =
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

zipViewDsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => Delimiter
        -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath
        -- ^ The path of a DSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

zipViewDsvFileStrictIgnoringAllErrors d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                zipViewPipeIgnoringAllErrors lu

zipViewDsvFileStrictThrowFirstError ::
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

zipViewDsvFileStrictThrowFirstError d fp lu =
    fmap snd $ liftIO $ runSafeT $
      do
        foldProducerM foldVectorM $
            withFile fp ReadMode $ \h ->
                void (handleDsvRowProducer d h) >->
                zipViewPipeThrowFirstError lu
