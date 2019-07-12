module Dsv.Cassava.Opts (CassavaOpts, delimCassavaOpts) where

import Dsv.DelimiterType

-- cassava
import qualified Data.Csv.Parser as Cassava

type CassavaOpts = Cassava.DecodeOptions

delimCassavaOpts
    :: Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> CassavaOpts

delimCassavaOpts d =
  Cassava.defaultDecodeOptions
    { Cassava.decDelimiter = delimiterWord8 d
    }
