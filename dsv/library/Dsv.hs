{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Dsv
  (
  -- * Reading CSV files strictly
  -- $readingCsvFilesStrictly
    readCsvFileStrictWithZippedHeader
  , readCsvFileStrictWithoutHeader
  , readCsvFileStrictIgnoringHeader

  -- * Reading DSV files strictly
  -- $readingDsvFilesStrictly
  , readDsvFileStrictWithZippedHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader

  -- * Reading CSV files strictly with any row type
  -- todo

  -- * Reading DSV files strictly with any row type
  , mapDsvFileStrictWithoutHeader
  , mapDsvFileStrictIgnoringHeader
  , mapDsvFileStrictUsingHeader

  -- * Folding CSV files
  -- $foldingCsvFiles
  , foldCsvFileWithZippedHeader, foldCsvFileWithZippedHeaderM
  , foldCsvFileWithoutHeader, foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader, foldCsvFileIgnoringHeaderM

  -- * Folding DSV files
  , foldDsvFileWithZippedHeader, foldDsvFileWithZippedHeaderM
  , foldDsvFileWithoutHeader, foldDsvFileWithoutHeaderM
  , foldDsvFileIgnoringHeader, foldDsvFileIgnoringHeaderM

  -- * Pipes that parse
  , csvRowPipe, dsvRowPipe

  -- * Producers from file handles
  , handleCsvRowProducer, handleDsvRowProducer

  -- * Combining a header with a row
  , zipHeader, zipHeader', zipHeaderPipe, applyHeaderPipe

  -- * Delimiters
  , Delimiter (..), comma, tab, delimiterWord8, charDelimiter

  -- * Attoparsec
  , attoPipe, handleAttoProducer, AttoError (..), AttoTermination (..)

  -- * Locating a column in a row
  , nthColumn, columnName

  -- * Miscellania
  -- $miscellania
  , byteStringTextUtf8Maybe
  , byteStringDecimalRationalMaybe
  , textDecimalRationalMaybe
  , byteStringDollarsMaybe
  , textDollarsMaybe

  ) where

import Dsv.AttoError
import Dsv.AttoParser
import Dsv.AttoPipe
import Dsv.AttoTermination
import Dsv.ByteString
import Dsv.Cassava
import Dsv.Delimiter
import Dsv.FileFold
import Dsv.FileStrictCsvRead
import Dsv.FileStrictMap
import Dsv.FileStrictRead
import Dsv.Header
import Dsv.Misc
import Dsv.Vector

import qualified Control.Foldl as L

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

{- $foldingCsvFiles

The functions in this section are all parameterized on:

  1. A 'FilePath', which specifies what CSV file to read;
  2. Either a 'L.Fold' or a 'L.FoldM', which specifies what action to take upon each row from the CSV file.

Use one of the functions with a 'L.Fold' parameter if you only need to collect information from the rows and aggregate it into some @result@ value. Use a function with a 'L.FoldM' parameter if your fold also needs to perform some kind of /effect/ as the rows are read from the file.

See the "Control.Foldl" module for much more on what folds are and how to construct them.

-}

{- $miscellania

These functions are not directly relevant to this library's primary purpose of consuming DSV files, but we include them because you might find some of them useful for reading particular kinds of values.

-}
