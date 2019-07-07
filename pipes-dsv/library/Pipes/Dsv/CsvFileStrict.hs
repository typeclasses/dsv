module Pipes.Dsv.CsvFileStrict
  ( readCsvFileStrictWithoutHeader
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileStrict
import Pipes.Dsv.Vector

readCsvFileStrictWithoutHeader :: FilePath -> IO (AttoTermination, [Vector ByteString])
readCsvFileStrictWithoutHeader fp = readDsvFileWithoutHeader comma fp

-- todo: readCsvFileStrictUsingHeader

-- todo: readCsvFileStrictIgnoringHeader
