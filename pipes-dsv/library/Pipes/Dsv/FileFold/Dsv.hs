module Pipes.Dsv.FileFold.Dsv
  ( foldDsvFileWithoutHeader
  , foldDsvFileWithoutHeaderM
  , foldDsvFileIgnoringHeader
  , foldDsvFileIgnoringHeaderM
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Cassava
import Pipes.Dsv.Delimiter
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

-- base
import Numeric.Natural (Natural)

-- foldl
import qualified Control.Foldl as Foldl
import Control.Foldl (Fold (Fold), FoldM (FoldM))

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

foldDsvFileIgnoringHeader
    :: MonadIO m
    => Delimiter -> FilePath
    -> Fold (Vector ByteString) a
    -> m (AttoTermination, a)

foldDsvFileIgnoringHeader d fp fld = foldDsvFileWithoutHeader d fp (foldDrop 1 fld)

foldDsvFileIgnoringHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter -> FilePath
    -> FoldM m (Vector ByteString) a
    -> m (AttoTermination, a)

foldDsvFileIgnoringHeaderM d fp fld = foldDsvFileWithoutHeaderM d fp (foldDropM 1 fld)

foldDrop :: Natural -> Fold a b -> Fold a b
foldDrop n (Fold step begin done) =
    Fold step' begin' done'
  where
    begin'          = (n, begin)
    step' (0,  s) x = (0, step s x)
    step' (n', s) _ = (n' - 1, s)
    done' (_,  s)   = done s

foldDropM :: Monad m => Natural -> FoldM m a b -> FoldM m a b
foldDropM n (FoldM step begin done) =
    FoldM step' begin' done'
  where
    begin'          = fmap (\s  -> (n, s))  begin
    step' (0,  s) x = fmap (\s' -> (0, s')) (step s x)
    step' (n', s) _ = return (n' - 1, s)
    done' (_,  s)   = done s
