module Pipes.Dsv
  (
  -- * Reading CSV files strictly
  -- $readingCsvFilesStrictly
    readCsvFileStrictUsingHeader
  , readCsvFileStrictWithoutHeader
  , readCsvFileStrictIgnoringHeader

  -- * Reading DSV files strictly
  -- $readingDsvFilesStrictly
  , readDsvFileStrictUsingHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader

  -- * Folding CSV files
  , foldCsvFileWithoutHeader
  , foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader
  , foldCsvFileIgnoringHeaderM

  -- * Folding DSV files
  , foldDsvFileWithoutHeader
  , foldDsvFileWithoutHeaderM
  , foldDsvFileIgnoringHeader
  , foldDsvFileIgnoringHeaderM

  -- * Labeled values
  , Labeled (..), zipNames, zipNames'

  -- * Delimiters
  , Delimiter (..), comma, tab, delimiterWord8, charDelimiter

  -- * Attoparsec
  , attoPipe, AttoError (..), AttoTermination (..)

  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileFold
import Pipes.Dsv.FileStrict
import Pipes.Dsv.Header

{- $readingCsvFilesStrictly

We present these functions first because they require the least amount of effort to use. Each function in this section:

  1. Assumes that the delimiter is a comma.
  2. Reads from a file (specified by a 'FilePath');
  3. Reads all of the results into memory at once ("strictly");

If you need to use a different delimiter, if your input source is something other than a file, or if you need streaming to control memory usage, read on to the next sections.

-}

{- $readingDsvFilesStrictly

\"CSV\" stands for "comma-separated values". But sometimes you may encounter CSV-like files in which the values are separated by some other character; e.g. it may have tabs instead of commas. We refer to such files more generally, then, as DSV files ("delimiter-separated values"). Functions that have a 'Delimiter' parameter, such as 'readDsvFileStrictWithoutHeader', let you specify what kind of DSV file you want to read.

-}
