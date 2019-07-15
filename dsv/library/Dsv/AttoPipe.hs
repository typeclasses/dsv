module Dsv.AttoPipe
  ( attoPipe
  , handleAttoProducer
  ) where

import Dsv.AttoError
import Dsv.AttoParser
import Dsv.AttoTermination
import Dsv.ByteString
import Dsv.IO

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

-- pipes
import Pipes

-- pipes-bytestring
import qualified Pipes.ByteString

attoPipe :: Monad m => AttoParser a -> Pipe ByteString a m AttoError
attoPipe p =
  do
    x <- await
    proceed (Atto.parse p x)

  where
    proceed (Atto.Partial c) =
      do
        x <- await
        proceed (c x)

    proceed (Atto.Done remainingInput x) =
      do
        yield x
        proceed (Atto.parse p remainingInput)

    proceed (Atto.Fail _remainingInput _ctxs _msg) =
        return AttoError

handleAttoProducer
    :: MonadIO m
    => AttoParser a
    -> Handle         -- ^ File handle to read parser input from
    -> Producer a m AttoTermination

handleAttoProducer p h = readBytes >-> parseRows
  where
    readBytes =
      do  Pipes.ByteString.fromHandle h
          return AttoComplete
    parseRows =
      do  _ <- attoPipe p
          return AttoIncomplete
