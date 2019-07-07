module Pipes.Dsv.CsvFileStrict
  ( readCsvFileStrictWithoutHeader
  , readCsvFileStrictUsingHeader
  , readCsvFileStrictIgnoringHeader
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileStrict
import Pipes.Dsv.Header
import Pipes.Dsv.Vector

readCsvFileStrictWithoutHeader :: FilePath -> IO (AttoTermination, [Vector ByteString])
readCsvFileStrictWithoutHeader fp = readDsvFileStrictWithoutHeader comma fp

readCsvFileStrictUsingHeader :: FilePath -> IO (AttoTermination, [Vector (Labeled ByteString ByteString)])
readCsvFileStrictUsingHeader fp = readDsvFileStrictUsingHeader comma fp

readCsvFileStrictIgnoringHeader :: FilePath -> IO (AttoTermination, [Vector ByteString])
readCsvFileStrictIgnoringHeader fp = readDsvFileStrictIgnoringHeader comma fp
