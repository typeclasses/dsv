{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |

DSV ("delimiter-separated values") is a simple file format used to save tabular data such as you might see in a spreadsheet. Each row is separated by a newline character, and the fields within each row are separated by the /delimiter/ (such as a comma, tab, etc.) Most often, the delimiter is a comma, in which case we call the file a CSV file ("comma-separated values").

For example, a CSV file might contain a list of expenses. We will use variations of the following example CSV file throughout the documentation:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

-}

module DSV
  (
  -- * Reading a CSV file as a Vector
  -- ** @readCsvFileStrict@...
  -- $readingCsvFilesStrictly
    readCsvFileStrictWithZippedHeader, readCsvFileStrictWithoutHeader, readCsvFileStrictIgnoringHeader
  -- ** What is a Vector
  -- $vector
  , Vector, nthVectorElement, vectorLookup, listToVector, vectorToList, emptyVector
  -- ** What is a ByteString
  -- $bytestring
  , ByteString
  -- ** A read ends with a ParseStop
  , ParseStop (..)

  -- * Other delimiters
  -- ** @readDsvFileStrict@...
  -- $readingDsvFilesStrictly
  , readDsvFileStrictWithZippedHeader, readDsvFileStrictWithoutHeader, readDsvFileStrictIgnoringHeader
  -- ** What is a Delimiter
  , Delimiter (..), comma, tab, delimiterWord8, charDelimiter

  -- * Reading with a custom row type
  -- ** @mapCsvFileStrict@...
  -- $readingCsvFilesStrictlyWithAnyRowType
  , mapCsvFileStrictWithoutHeader, mapCsvFileStrictIgnoringHeader, mapCsvFileStrictUsingHeader
  -- ** Using other delimiters
  -- $readingDsvFilesStrictlyWithAnyRowType
  , mapDsvFileStrictWithoutHeader, mapDsvFileStrictIgnoringHeader, mapDsvFileStrictUsingHeader

  -- * Iterating over a file with a Fold
  -- ** @foldCsvFile@...
  -- $foldingCsvFiles
  , foldCsvFileWithZippedHeader, foldCsvFileWithZippedHeaderM
  , foldCsvFileWithoutHeader, foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader, foldCsvFileIgnoringHeaderM
  -- ** What is a Fold
  -- $fold
  , Fold (..), FoldM (..)
  -- ** Using other delimiters
  -- $foldingDsvFiles
  , foldDsvFileWithZippedHeader, foldDsvFileWithZippedHeaderM
  , foldDsvFileWithoutHeader, foldDsvFileWithoutHeaderM
  , foldDsvFileIgnoringHeader, foldDsvFileIgnoringHeaderM

  -- * Functions that can fail
  -- ** What is a View
  , View (..)
  -- ** What is Validation
  -- $validation
  , Validation (..)
  -- ** Constructing views
  , constView, maybeView
  -- ** Modifying views
  , overViewError, discardViewError
  -- ** Composing views
  -- $composingViews
  , (>>>), (<<<)
  , (>>>-), (<<<-)
  -- ** Using views
  , applyView, viewMaybe, viewOr
  -- ** Viewing strings as numbers
  , byteStringNatView, textNatView, InvalidNat (..)
  , byteStringRationalView, textRationalView, InvalidRational (..)
  , byteStringDollarsView, textDollarsView, InvalidDollars (..)

  -- ** Viewing a position of a vector
  , columnNumberView, TooShort (..), IndexError (..)
  -- ** Finding something in a vector
  , lookupView, Duplicate (..), Missing (..), LookupError (..)

  -- * Header-and-row views
  -- ** What is a ZipView
  , ZipView (..)
  -- ** Basic zip view operations
  , overZipViewError, overHeaderError, overRowError
  -- ** Converting a ZipView to a Pipe
  , zipViewPipe, zipViewPipeIgnoringAllErrors, zipViewPipeThrowFirstError
  -- ** Some zip views
  , byteStringZipView, textZipViewUtf8, textZipViewUtf8', byteStringZipViewPosition, entireRowZipView
  -- ** Refining a ZipView with a View
  , refineZipView
  -- ** Combining a ZipView with a Fold
  , zipViewFold, zipViewFoldM, ZipViewError (..)
  -- ** Reading strictly from CSV files using ZipView
  , zipViewCsvFileStrict
  , zipViewCsvFileStrictIgnoringAllErrors
  , zipViewCsvFileStrictThrowFirstError
  -- ** A read ends with a ZipViewStop
  , ZipViewStop (..)
  -- ** Using other delimiters
  , zipViewDsvFileStrict
  , zipViewDsvFileStrictIgnoringAllErrors
  , zipViewDsvFileStrictThrowFirstError

  -- * Pipes
  -- ** Pipes that parse DSV rows
  , csvRowPipe, dsvRowPipe
  -- ** Creating row producers from file handles
  , handleCsvRowProducer, handleDsvRowProducer
  -- ** Pipes that combine the header with subsequent rows
  , zipHeaderPipe, zipHeaderWithPipe
  -- ** What are Pipes
  -- $pipes
  , Pipe, Producer, Consumer, Effect, runEffect, (>->), await, yield

  -- * Attoparsec
  -- $attoparsec
  , AttoParser, attoPipe, handleAttoProducer, ParseError (..)

  -- * Position types
  , Position (..), RowNumber (..), ColumnNumber (..)
  , ColumnName (..), Positive (..), At (..)

  -- * Text
  -- ** What is Text
  -- $text
  , Text
  -- ** Relationship to String
  , stringToText, textToString
  -- ** Relationship to Bytestring
  , encodeTextUtf8, utf8TextView, InvalidUtf8 (..)

  ) where

import DSV.AttoParser
import DSV.AttoPipe
import DSV.ByteString
import DSV.CommonDelimiters
import DSV.DelimiterSplice
import DSV.DelimiterType
import DSV.UTF8
import DSV.FileFold
import DSV.FileFoldCsv
import DSV.FileStrictCsvMap
import DSV.FileStrictCsvRead
import DSV.FileStrictCsvZipView
import DSV.FileStrictMap
import DSV.FileStrictRead
import DSV.FileStrictZipView
import DSV.Fold
import DSV.Header
import DSV.IndexError
import DSV.LookupError
import DSV.ZipViews
import DSV.Numbers
import DSV.NumberViews
import DSV.ParseError
import DSV.ParseStop
import DSV.Parsing
import DSV.Pipes
import DSV.Position
import DSV.Prelude
import DSV.Text
import DSV.Validation
import DSV.Vector
import DSV.VectorViews
import DSV.ViewType
import DSV.ZipViewError
import DSV.ZipViewFold
import DSV.ZipViewPipe
import DSV.ZipViewStop
import DSV.ZipViewType

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

{- $miscellania

These functions are not directly relevant to this library's primary purpose of consuming DSV files, but we include them because you might find some of them useful for reading particular kinds of values.

-}

{- $validation

See the "Data.Validation" module for more on the 'Validation' type.

-}

{- $vector

See the "Data.Vector" module for more on the 'Vector' type.

-}

{- $bytestring

See the "Data.ByteString" module for more on the 'ByteString' type.

-}

{- $fold

See the "Control.Foldl" module for more on the 'Fold' and 'FoldM' types.

-}

{- $text

See the "Data.Text" module for more on the 'Text' type.

-}

{- $attoparsec

See the "Data.Attoparsec.ByteString" module for more on parsing byte strings.

-}

{- $pipes

See the "Pipes" module for more on pipes.

-}

{- $composingViews

'View' has a 'Category' instance, so you can chain views together using '>>>' and '<<<'. See the "Control.Category" module for more on categories.

The two views being sequenced have to have the same error type, which is often inconvenient. To chain views together while converting their error type to @()@, you can use '>>>-' and '<<<-' instead.

-}
