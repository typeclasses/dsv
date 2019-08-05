{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.Parsing
  ( dsvRowAtto
  , dsvRowPipe, csvRowPipe
  , handleCsvRowProducer, handleDsvRowProducer
  ) where

import Dsv.AttoParser
import Dsv.AttoPipe
import Dsv.ByteString
import Dsv.CommonDelimiters
import Dsv.IO
import Dsv.ParseError
import Dsv.ParseTermination
import Dsv.Prelude
import Dsv.Vector

-- attoparsec
import Data.Attoparsec.ByteString.Char8 (endOfLine)

-- cassava
import qualified Data.Csv.Parser as Cassava

-- pipes
import Pipes

dsvRowAtto ::
    Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> AttoParser (Vector ByteString)

dsvRowAtto d =
    Cassava.record (delimiterWord8 d) <* endOfLine

dsvRowPipe ::
    forall m .
    Monad m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Pipe ByteString (Vector ByteString) m ParseError

dsvRowPipe d =
    attoPipe (dsvRowAtto d)

-- | This pipe 'await's @ByteString@ input read from a CSV file, parses the input, and 'yield's a @Vector ByteString@ for each row in the CSV file. If this pipe reaches some portion of the input that is not formatted correctly and cannot parse any further, the pipe terminates and 'return's an @ParseError@.

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
    -> Producer (Vector ByteString) m ParseTermination

handleCsvRowProducer h =
    handleDsvRowProducer comma h

handleDsvRowProducer ::
    forall m .
    MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Handle     -- ^ File handle to read DSV data from
    -> Producer (Vector ByteString) m ParseTermination

handleDsvRowProducer d h =
    handleAttoProducer (dsvRowAtto d) h
