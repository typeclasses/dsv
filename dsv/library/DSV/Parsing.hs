{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.Parsing
  ( dsvRowAtto
  , dsvRowPipe, csvRowPipe
  , handleCsvRowProducer, handleDsvRowProducer
  ) where

import DSV.AttoParser
import DSV.AttoPipe
import DSV.ByteString
import DSV.CommonDelimiters
import DSV.IO
import DSV.ParseError
import DSV.ParseStop
import DSV.Pipes
import DSV.Prelude
import DSV.Vector

-- attoparsec
import Data.Attoparsec.ByteString.Char8 (endOfLine)

-- cassava
import qualified Data.Csv.Parser as Cassava

dsvRowAtto ::
    Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> AttoParser (Vector ByteString)

dsvRowAtto d =
    Cassava.record (delimiterWord8 d) <* endOfLine

-- | Like 'csvRowPipe', but allows customizing the delimiter.

dsvRowPipe ::
    forall m .
    Monad m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Pipe ByteString (Vector ByteString) m ParseError

dsvRowPipe d =
    attoPipe (dsvRowAtto d)

-- | This pipe 'await's @ByteString@ input read from a CSV file, parses the input, and 'yield's a @'Vector' 'ByteString'@ for each row in the CSV file. If this pipe reaches some portion of the input that is not formatted correctly and cannot parse any further, the pipe terminates and 'return's a 'ParseError'.

csvRowPipe ::
    forall m .
    Monad m
    => Pipe ByteString (Vector ByteString) m ParseError

csvRowPipe =
    attoPipe (dsvRowAtto comma)

handleCsvRowProducer ::
    forall m .
    MonadIO m
    => Handle     -- ^ File handle to read CSV data from
    -> Producer (Vector ByteString) m ParseStop

handleCsvRowProducer h =
    handleDsvRowProducer comma h

handleDsvRowProducer ::
    forall m .
    MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Handle     -- ^ File handle to read DSV data from
    -> Producer (Vector ByteString) m ParseStop

handleDsvRowProducer d h =
    handleAttoProducer (dsvRowAtto d) h
