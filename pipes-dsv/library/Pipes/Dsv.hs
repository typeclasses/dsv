module Pipes.Dsv
  (
  -- * Reading CSV files strictly
  -- $readingFilesStrictly
    readCsvFileStrictWithoutHeader
  , readCsvFileStrictUsingHeader
  , readCsvFileStrictIgnoringHeader

  -- * Labeled valued
  , Labeled (..), zipNames, zipNames'

  -- * Delimiters

  -- ** The Delimiter type
  -- $delimiters
  , Delimiter (..), comma, tab, delimiterWord8, charDelimiter

  -- ** Reading DSV files strictly
  , readDsvFileStrictWithoutHeader, readDsvFileStrictUsingHeader

  -- * Attoparsec
  , attoPipe, AttoError (..), AttoTermination (..)

  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.CsvFileStrict
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileStrict
import Pipes.Dsv.Header

{- $readingFilesStrictly

We present these functions first because they require the least amount of effort to use. Each function in this section:

  1. Assumes that the delimiter is a comma.
  2. Reads from a file (specified by a 'FilePath');
  3. Reads all of the results into memory at once ("strictly");

If you need to use a different delimiter, if your input source is something other than a file, or if you need streaming to control memory usage, read on to the next sections.

-}

{- $delimiters

"CSV" stands for "comma-separated values". But sometimes you may encounter CSV-like files in which the values are separated by some other character; e.g. it may have tabs instead of commas. We refer to such files more generally, then, as DSV files ("delimiter-separated values"). Functions that have a 'Delimiter' parameter, such as 'readDsvStrictFileWithoutHeader', let you specify what kind of DSV file you want to read.

-}
