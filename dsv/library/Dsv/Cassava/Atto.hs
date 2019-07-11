module Dsv.Cassava.Atto (dsvRowAtto) where

import Dsv.AttoParser
import Dsv.ByteString
import Dsv.Delimiter
import Dsv.Vector

-- attoparsec
import Data.Attoparsec.ByteString.Char8 (endOfLine)

-- cassava
import qualified Data.Csv.Parser as Cassava

dsvRowAtto
    :: Delimiter  -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> AttoParser (Vector ByteString)

dsvRowAtto d = Cassava.record (delimiterWord8 d) <* endOfLine
