module Dsv.Atto
  ( attoPipe, handleAttoProducer
  , AttoError (..)
  , AttoParser
  , AttoTermination (..)
  ) where

import Dsv.Atto.Error
import Dsv.Atto.Pipe
import Dsv.Atto.Termination

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

type AttoParser = Atto.Parser
