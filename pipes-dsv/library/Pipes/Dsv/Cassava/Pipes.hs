module Pipes.Dsv.Cassava.Pipes (dsvRowPipe) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Cassava.Atto
import Pipes.Dsv.Delimiter
import Pipes.Dsv.Vector

-- pipes
import Pipes

dsvRowPipe :: Monad m => Delimiter -> Pipe ByteString (Vector ByteString) m AttoError
dsvRowPipe d = attoPipe (dsvRowAtto d)
