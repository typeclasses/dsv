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
import Pipes.Dsv.Fold
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

-- pipes
import Pipes

-- pipes-safe
import qualified Pipes.Safe.Prelude as P

foldDsvFileWithoutHeader
    :: MonadIO m
    => Delimiter                      -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath                       -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) a     -- ^ What to do with each row
    -> m (AttoTermination, a)

foldDsvFileWithoutHeader d fp fld =
    liftIO $ runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducer fld (handleAttoProducer (dsvRowAtto d) h)

foldDsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter                      -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath                       -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) a  -- ^ What to do with each row
    -> m (AttoTermination, a)

foldDsvFileWithoutHeaderM d fp fld =
    runSafeT $ P.withFile fp ReadMode $ \h -> lift $
        foldProducerM fld (handleAttoProducer (dsvRowAtto d) h)

foldDsvFileIgnoringHeader
    :: MonadIO m
    => Delimiter                      -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath                       -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) a     -- ^ What to do with each row
    -> m (AttoTermination, a)

foldDsvFileIgnoringHeader d fp fld =
    foldDsvFileWithoutHeader d fp (foldDrop 1 fld)

foldDsvFileIgnoringHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => Delimiter                      -- ^ What character separates input values, e.g. 'comma' or 'tab'
    -> FilePath                       -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) a  -- ^ What to do with each row
    -> m (AttoTermination, a)

foldDsvFileIgnoringHeaderM d fp fld =
    foldDsvFileWithoutHeaderM d fp (foldDropM 1 fld)
