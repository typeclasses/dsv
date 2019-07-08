module Pipes.Dsv.FileFold.Dsv
  ( foldDsvFileWithoutHeader
  , foldDsvFileWithoutHeaderM
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Cassava
import Pipes.Dsv.Delimiter
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

-- foldl
import qualified Control.Foldl as Foldl
import Control.Foldl (Fold, FoldM)

-- pipes
import Pipes
import qualified Pipes.Prelude as P

-- pipes-safe
import qualified Pipes.Safe.Prelude as P

foldDsvFileWithoutHeader
    :: MonadIO m
    => Delimiter -> FilePath
    -> Fold (Vector ByteString) a
    -> m (AttoTermination, a)

foldDsvFileWithoutHeader d fp fld =
    liftIO $ runSafeT $ P.withFile fp ReadMode $ \h -> lift $
      do
        (x, t) <-
            Foldl.purely P.fold' fld $
                handleAttoProducer (dsvRowAtto d) h
        return (t, x)

foldDsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter -> FilePath
    -> FoldM m (Vector ByteString) a
    -> m (AttoTermination, a)

foldDsvFileWithoutHeaderM d fp fld =
    runSafeT $ P.withFile fp ReadMode $ \h -> lift $
      do
        (x, t) <-
            Foldl.impurely P.foldM' fld $
                handleAttoProducer (dsvRowAtto d) h
        return (t, x)
