module Pipes.Dsv.Cassava.Pipes (dsvRowPipe, csvRowPipe) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Cassava.Atto
import Pipes.Dsv.Delimiter
import Pipes.Dsv.Vector

-- pipes
import Pipes

dsvRowPipe
    :: Monad m
    => Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> Pipe ByteString (Vector ByteString) m AttoError

dsvRowPipe d = attoPipe (dsvRowAtto d)

-- | This pipe 'await's @ByteString@ input read from a CSV file, parses the input, and 'yield's a @Vector ByteString@ for each row in the CSV file. If this pipe reaches some portion of the input that is not formatted correctly and cannot parse any further, the pipe terminates and 'return's an @AttoError@.

csvRowPipe
    :: Monad m
    => Pipe ByteString (Vector ByteString) m AttoError

csvRowPipe = attoPipe (dsvRowAtto comma)
