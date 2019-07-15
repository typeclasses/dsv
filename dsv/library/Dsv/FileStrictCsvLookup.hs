{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileStrictCsvLookup
  ( lookupCsvFileStrict
  , lookupCsvFileStrictIgnoringAllErrors
  , lookupCsvFileStrictThrowFirstError
  ) where

import Dsv.AttoLookupTermination
import Dsv.CommonDelimiters
import Dsv.FileStrictLookup
import Dsv.IO
import Dsv.LookupType
import Dsv.Validation
import Dsv.Vector

lookupCsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => FilePath
    -> Lookup headerError rowError row
    -> m (AttoLookupTermination headerError, Vector (Validation rowError row))

lookupCsvFileStrict fp lu =
    lookupDsvFileStrict comma fp lu

lookupCsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => FilePath
    -> Lookup headerError rowError row
    -> m (Vector row)

lookupCsvFileStrictIgnoringAllErrors fp lu =
    lookupDsvFileStrictIgnoringAllErrors comma fp lu

lookupCsvFileStrictThrowFirstError ::
    forall m headerError rowError row .
    (MonadIO m, Exception headerError, Exception rowError)
    => FilePath
    -> Lookup headerError rowError row
    -> m (Vector row)

lookupCsvFileStrictThrowFirstError fp lu =
    lookupDsvFileStrictThrowFirstError comma fp lu
