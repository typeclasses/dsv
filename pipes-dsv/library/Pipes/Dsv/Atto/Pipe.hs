module Pipes.Dsv.Atto.Pipe (attoPipe, handleAttoProducer) where

import Pipes.Dsv.Atto.Error
import Pipes.Dsv.Atto.Termination
import Pipes.Dsv.ByteString
import Pipes.Dsv.IO

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

-- pipes
import Pipes

-- pipes-bytestring
import qualified Pipes.ByteString

attoPipe :: Monad m => Atto.Parser a -> Pipe ByteString a m AttoError
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

    proceed (Atto.Fail _remainingInput ctxs msg) =
        return (AttoError ctxs msg)

handleAttoProducer :: MonadIO m => Atto.Parser a -> Handle -> Producer a m AttoTermination
handleAttoProducer p h = readBytes >-> parseRows
  where
    readBytes =
      do  Pipes.ByteString.fromHandle h
          return AttoComplete
    parseRows =
      do  e <- attoPipe p
          return (AttoIncomplete e)
