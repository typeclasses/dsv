{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.FileStrictCsvMap
  ( mapCsvFileStrictWithoutHeader
  , mapCsvFileStrictIgnoringHeader
  , mapCsvFileStrictUsingHeader
  ) where

import DSV.ByteString
import DSV.CommonDelimiters
import DSV.FileStrictMap
import DSV.IO
import DSV.ParseStop
import DSV.Prelude
import DSV.Vector

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
