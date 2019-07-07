module Pipes.Dsv.CsvFileStrict
  ( readCsvFileStrictWithoutHeader
  , readCsvFileStrictUsingHeader
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileStrict
import Pipes.Dsv.Header
import Pipes.Dsv.Vector

readCsvFileStrictWithoutHeader :: FilePath -> IO (AttoTermination, [Vector ByteString])
readCsvFileStrictWithoutHeader fp = readDsvFileWithoutHeader comma fp

readCsvFileStrictUsingHeader :: FilePath -> IO (AttoTermination, [Vector (Labeled ByteString ByteString)])
readCsvFileStrictUsingHeader fp = readDsvFileUsingHeader comma fp

-- todo: readCsvFileStrictIgnoringHeader
