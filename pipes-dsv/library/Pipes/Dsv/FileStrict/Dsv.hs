module Pipes.Dsv.FileStrict.Dsv
  ( readDsvFileStrictUsingHeader
  , readDsvFileStrictWithoutHeader
  , readDsvFileStrictIgnoringHeader
  ) where

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

readDsvFileStrictWithoutHeader :: Delimiter -> FilePath -> IO (AttoTermination, [Vector ByteString])
readDsvFileStrictWithoutHeader d fp =
    runSafeT $
      do
        (xs, t) <- P.toListM' $
            P.withFile fp ReadMode (handleAttoProducer (dsvRowAtto d))
        return (t, xs)

readDsvFileStrictUsingHeader :: Delimiter -> FilePath -> IO (AttoTermination, [Vector (Labeled ByteString ByteString)])
readDsvFileStrictUsingHeader d fp =
    fmap (fmap zipNames) (readDsvFileStrictWithoutHeader d fp)

readDsvFileStrictIgnoringHeader :: Delimiter -> FilePath -> IO (AttoTermination, [Vector ByteString])
readDsvFileStrictIgnoringHeader d fp =
    fmap (fmap (drop 1)) (readDsvFileStrictWithoutHeader d fp)
