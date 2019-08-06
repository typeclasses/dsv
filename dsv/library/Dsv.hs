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
  -- $readingCsvFilesStrictlyWithAnyRowType
  , mapCsvFileStrictWithoutHeader
  , mapCsvFileStrictIgnoringHeader
  , mapCsvFileStrictUsingHeader

  -- * Reading DSV files strictly with any row type
  -- $readingDsvFilesStrictlyWithAnyRowType
  , mapDsvFileStrictWithoutHeader
  , mapDsvFileStrictIgnoringHeader
  , mapDsvFileStrictUsingHeader

  -- * Folding CSV files
  -- $foldingCsvFiles
  , foldCsvFileWithZippedHeader, foldCsvFileWithZippedHeaderM
  , foldCsvFileWithoutHeader, foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader, foldCsvFileIgnoringHeaderM

  -- * Folding DSV files
  -- $foldingDsvFiles
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

  -- * Parsing-related types
  , ParseError (..)

  -- * Attoparsec
  , AttoParser, attoPipe, handleAttoProducer

  -- * Locating a column in a row
  -- todo: rethink these names in light of the new Lookup stuff
  , nthColumn, columnName

  -- * The ZipView type
  , ZipView (..)
  , mapZipViewError, overHeaderError, mapRowError

  -- * Converting zip views to pipes
  , zipViewPipe
  , zipViewPipeIgnoringAllErrors
  , zipViewPipeThrowFirstError

  -- * Some zip views
  , byteStringZipView, textZipViewUtf8, textZipViewUtf8', byteStringZipViewPosition, entireRowZipView

  -- * The View type
  , View (..)

  -- * Some views
  , constView, utf8View, byteStringNatView, textNatView

  -- * ZipView + View
  , refineZipView

  -- * Lookup errors
  -- $lookupErrors
  , InvalidUtf8 (..)
  , InvalidNat (..)
  , Missing (..)
  , Duplicate (..)
  , LookupError (..)
  , TooShort (..)
  , IndexError (..)

  -- * English
  , EnglishText (..)

  -- * Reading strictly from CSV files using ZipView
  , zipViewCsvFileStrict
  , zipViewCsvFileStrictIgnoringAllErrors
  , zipViewCsvFileStrictThrowFirstError

  -- * Reading strictly from DSV files using ZipView
  , zipViewDsvFileStrict
  , zipViewDsvFileStrictIgnoringAllErrors
  , zipViewDsvFileStrictThrowFirstError

  -- * Termination types
  , ParseTermination (..), ZipViewStop (..)

  -- * Position types
  , Position (..), RowNumber (..), ColumnNumber (..)
  , ColumnName (..), Positive (..), At (..)

  -- * Miscellania
  -- $miscellania
  , byteStringTextUtf8Maybe
  , byteStringDecimalRationalMaybe
  , textDecimalRationalMaybe
  , byteStringDollarsMaybe
  , textDollarsMaybe

  -- * Validation
  -- $validation
  , Validation (..)

  -- * Text encoding
  , EncodeUtf8 (..), DecodeUtf8 (..)

  ) where

import Dsv.AttoParser
import Dsv.AttoPipe
import Dsv.ByteString
import Dsv.CommonDelimiters
import Dsv.DelimiterSplice
import Dsv.DelimiterType
import Dsv.Encoding
import Dsv.English
import Dsv.FileFold
import Dsv.FileFoldCsv
import Dsv.FileStrictCsvMap
import Dsv.FileStrictCsvRead
import Dsv.FileStrictCsvZipView
import Dsv.FileStrictMap
import Dsv.FileStrictRead
import Dsv.FileStrictZipView
import Dsv.Header
import Dsv.LookupErrors
import Dsv.ZipViews
import Dsv.Misc
import Dsv.Numbers
import Dsv.NumberViews
import Dsv.ParseError
import Dsv.ParseTermination
import Dsv.Parsing
import Dsv.Position
import Dsv.Validation
import Dsv.Vector
import Dsv.Views
import Dsv.ViewType
import Dsv.ZipViewPipe
import Dsv.ZipViewStop
import Dsv.ZipViewType

import qualified Control.Foldl as L

{- $readingCsvFilesStrictly

We present these functions first because they require the least amount of effort to use. Each function in this section:

  1. Assumes that the delimiter is a comma.
  2. Reads from a file (specified by a 'FilePath');
  3. Reads all of the results into memory at once ("strictly");

Read on to the subsequent sections if:

  - you need to use a different delimiter;
  - your input source is something other than a file;
  - you need streaming to control memory usage; or
  - you would like assistance in converting the data from 'Vector's of 'ByteString's to other types.

-}

{- $readingDsvFilesStrictly

\"CSV\" stands for "comma-separated values". But sometimes you may encounter CSV-like files in which the values are separated by some other character; e.g. it may have tabs instead of commas. We refer to such files more generally, then, as DSV files ("delimiter-separated values"). Functions that have a 'Delimiter' parameter, such as 'readDsvFileStrictWithoutHeader', let you specify what kind of DSV file you want to read.

-}

{- $readingCsvFilesStrictlyWithAnyRowType

Most likely, you don't just want to get 'Vector's of 'ByteString' values from a CSV file; you want to interpret the meaning of those bytes somehow, converting each row into some type that is specific to the kind of data that your particular CSV file represents. These functions are parameterized on a function of type @(Vector ByteString -> IO row)@ which will get applied to each row as it is read. Then instead of getting each row as a @Vector ByteString@, each row will be represented in the result as a value of type @row@ (where @row@ is a type parameter that stands for whatever type your conversion function returns).

-}

{- $readingDsvFilesStrictlyWithAnyRowType

This section is the same as the previous, but generalized with a 'Delimiter' parameter.

-}

{- $foldingCsvFiles

The functions in this section are all parameterized on:

  1. A 'FilePath', which specifies what CSV file to read;
  2. Either a 'L.Fold' or a 'L.FoldM', which specifies what action to take upon each row from the CSV file.

Use one of the functions with a 'L.Fold' parameter if you only need to collect information from the rows and aggregate it into some @result@ value. Use a function with a 'L.FoldM' parameter if your fold also needs to perform some kind of /effect/ as the rows are read from the file.

See the "Control.Foldl" module for much more on what folds are and how to construct them.

-}

{- $foldingDsvFiles

This section is the same as the previous, but generalized with a 'Delimiter' parameter.

-}

{- $lookupErrors

When you're looking for particular information in DSV data, there is a lot that can go wrong. The columns that you want might not be there, or the strings that you find in the rows may not be correctly formatted.

-}

{- $miscellania

These functions are not directly relevant to this library's primary purpose of consuming DSV files, but we include them because you might find some of them useful for reading particular kinds of values.

-}

{- $validation

See the "Data.Validation" module for more on the 'Validation' type.

-}
