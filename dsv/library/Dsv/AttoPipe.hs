{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.AttoPipe
  ( attoPipe
  , handleAttoProducer
  ) where

import Dsv.AttoParser
import Dsv.ByteString
import Dsv.IO
import Dsv.ParseError
import Dsv.ParseTermination
import Dsv.Prelude

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

-- pipes
import Pipes

-- pipes-bytestring
import qualified Pipes.ByteString

attoPipe :: forall a m .
    Monad m
    => AttoParser a
    -> Pipe ByteString a m ParseError

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
        return ParseError

handleAttoProducer ::
    forall a m .
    MonadIO m
    => AttoParser a
    -> Handle         -- ^ File handle to read parser input from
    -> Producer a m ParseTermination

handleAttoProducer p h = readBytes >-> parseRows
  where
    readBytes =
      do  Pipes.ByteString.fromHandle h
          return ParseComplete
    parseRows =
      do  _ <- attoPipe p
          return ParseIncomplete
