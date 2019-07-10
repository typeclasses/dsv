module Pipes.Dsv.Atto
  ( attoPipe, handleAttoProducer
  , AttoError (..)
  , AttoParser
  , AttoTermination (..)
  ) where

import Pipes.Dsv.Atto.Error
import Pipes.Dsv.Atto.Pipe
import Pipes.Dsv.Atto.Termination

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

type AttoParser = Atto.Parser
