module Pipes.Dsv.Cassava.Opts (CassavaOpts, delimCassavaOpts) where

import Pipes.Dsv.Delimiter

-- cassava
import qualified Data.Csv.Parser as Cassava

type CassavaOpts = Cassava.DecodeOptions

delimCassavaOpts :: Delimiter -> CassavaOpts
delimCassavaOpts d =
  Cassava.defaultDecodeOptions
    { Cassava.decDelimiter = delimiterWord8 d
    }
