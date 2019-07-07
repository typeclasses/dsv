module Pipes.Dsv.Cassava.Atto (dsvRowAtto) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.Vector

-- attoparsec
import Data.Attoparsec.ByteString.Char8 (endOfLine)

-- cassava
import qualified Data.Csv.Parser as Cassava

dsvRowAtto :: Delimiter -> AttoParser (Vector ByteString)
dsvRowAtto d = Cassava.record (delimiterWord8 d) <* endOfLine
