{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileStrictCsvMap
  ( mapCsvFileStrictWithoutHeader
  , mapCsvFileStrictIgnoringHeader
  , mapCsvFileStrictUsingHeader
  ) where

import Dsv.ByteString
import Dsv.CommonDelimiters
import Dsv.FileStrictMap
import Dsv.IO
import Dsv.ParseStop
import Dsv.Prelude
import Dsv.Vector

mapCsvFileStrictWithoutHeader ::
    forall m row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the CSV file
    -> m (ParseStop, Vector row)

mapCsvFileStrictWithoutHeader fp f =
    mapDsvFileStrictWithoutHeader comma fp f

mapCsvFileStrictIgnoringHeader ::
    forall m row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the CSV file
    -> m (ParseStop, Vector row)

mapCsvFileStrictIgnoringHeader fp f =
    mapDsvFileStrictIgnoringHeader comma fp f

mapCsvFileStrictUsingHeader ::
    forall m row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO (Vector ByteString -> IO row))
        -- ^ Function which interprets the header (the first @Vector ByteString@) and returns a conversion function (@Vector ByteString -> IO row@) by which you specify how to interpret one row of bytes from the CSV file
    -> m (ParseStop, Vector row)

mapCsvFileStrictUsingHeader fp f =
    mapDsvFileStrictUsingHeader comma fp f
