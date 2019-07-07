module Pipes.Dsv.FileStrict (readDsvFileWithoutHeader, readDsvFileUsingHeader) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Cassava
import Pipes.Dsv.Delimiter
import Pipes.Dsv.Header
import Pipes.Dsv.Vector

-- base
import System.IO (IOMode (ReadMode))

-- pipes
import qualified Pipes.Prelude as P

-- pipes-safe
import qualified Pipes.Safe.Prelude as P
import Pipes.Safe (runSafeT)

readDsvFileWithoutHeader :: Delimiter -> FilePath -> IO (AttoTermination, [Vector ByteString])
readDsvFileWithoutHeader d fp =
    runSafeT $
      do
        (xs, t) <- P.toListM' $
            P.withFile fp ReadMode (handleAttoProducer (dsvRowAtto d))
        return (t, xs)

readDsvFileUsingHeader :: Delimiter -> FilePath -> IO (AttoTermination, [Vector (Labeled ByteString ByteString)])
readDsvFileUsingHeader d fp =
    fmap (fmap zipNames) (readDsvFileWithoutHeader d fp)
