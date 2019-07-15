{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Dsv.FileStrictLookup
  ( lookupDsvFileStrict
  , lookupDsvFileStrictIgnoringAllErrors
  , lookupDsvFileStrictThrowFirstError
  ) where

import Dsv.DelimiterType
import Dsv.Fold
import Dsv.IO
import Dsv.LookupPipe
import Dsv.LookupType
import Dsv.ParseTermination
import Dsv.ParseLookupTermination
import Dsv.Parsing
import Dsv.Validation
import Dsv.Vector

-- base
import Numeric.Natural

-- pipes
import Pipes

count :: Monad m => Producer a m r -> Producer a m (Natural, r)
count = go 0
  where
    go n p =
      do
        eit <- lift (next p)
        case eit of
            Left r -> return (n, r)
            Right (a, p') -> do { yield a; go (n + 1) p' }

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
                readRows h >-> interpretRows

  where
    readRows h =
      fmap
        (
          \case
            (_, ParseIncomplete) -> ParseLookupParseError
            (0, ParseComplete) -> ParseLookupEmpty
            (_, ParseComplete) -> ParseLookupComplete
        )
        (count (handleDsvRowProducer d h))

    interpretRows =
      fmap ParseLookupHeaderError (lookupPipe lu)

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
