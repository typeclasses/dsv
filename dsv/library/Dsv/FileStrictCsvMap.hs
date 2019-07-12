module Dsv.FileStrictCsvMap
  ( mapCsvFileStrictWithoutHeader
  , mapCsvFileStrictIgnoringHeader
  , mapCsvFileStrictUsingHeader
  ) where

import Dsv.AttoTermination
import Dsv.ByteString
import Dsv.CommonDelimiters
import Dsv.FileStrictMap
import Dsv.IO
import Dsv.Vector

mapCsvFileStrictWithoutHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the CSV file
    -> m (AttoTermination, Vector row)

mapCsvFileStrictWithoutHeader fp f =
    mapDsvFileStrictWithoutHeader comma fp f

mapCsvFileStrictIgnoringHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO row)
        -- ^ Conversion function by which you specify how to interpret one row of bytes from the CSV file
    -> m (AttoTermination, Vector row)

mapCsvFileStrictIgnoringHeader fp f =
    mapDsvFileStrictIgnoringHeader comma fp f

mapCsvFileStrictUsingHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> (Vector ByteString -> IO (Vector ByteString -> IO row))
        -- todo: doc
    -> m (AttoTermination, Vector row)

mapCsvFileStrictUsingHeader fp f =
    mapDsvFileStrictUsingHeader comma fp f
