module Dsv.Cassava.Pipes
  ( dsvRowPipe, csvRowPipe
  , handleCsvRowProducer, handleDsvRowProducer
  ) where

import Dsv.Atto
import Dsv.ByteString
import Dsv.Cassava.Atto
import Dsv.Delimiter
import Dsv.IO
import Dsv.Vector

-- pipes
import Pipes

dsvRowPipe
    :: Monad m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Pipe ByteString (Vector ByteString) m AttoError

dsvRowPipe d =
    attoPipe (dsvRowAtto d)

-- | This pipe 'await's @ByteString@ input read from a CSV file, parses the input, and 'yield's a @Vector ByteString@ for each row in the CSV file. If this pipe reaches some portion of the input that is not formatted correctly and cannot parse any further, the pipe terminates and 'return's an @AttoError@.

csvRowPipe
    :: Monad m
    => Pipe ByteString (Vector ByteString) m AttoError

csvRowPipe =
    attoPipe (dsvRowAtto comma)

handleCsvRowProducer
    :: MonadIO m
    => Handle     -- ^ File handle to read CSV data from
    -> Producer (Vector ByteString) m AttoTermination

handleCsvRowProducer h =
    handleDsvRowProducer comma h

handleDsvRowProducer
    :: MonadIO m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Handle     -- ^ File handle to read DSV data from
    -> Producer (Vector ByteString) m AttoTermination

handleDsvRowProducer d h =
    handleAttoProducer (dsvRowAtto d) h
