module Pipes.Dsv.FileFold.Csv
  ( foldCsvFileWithoutHeader
  , foldCsvFileWithoutHeaderM
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileFold.Dsv
import Pipes.Dsv.Vector

-- base
import Control.Monad.IO.Class

-- foldl
import Control.Foldl (Fold, FoldM)

-- pipes-safe
import Pipes.Safe (MonadCatch, MonadMask)

foldCsvFileWithoutHeader
    :: MonadIO m
    => FilePath
    -> Fold (Vector ByteString) a
    -> m (AttoTermination, a)

foldCsvFileWithoutHeader fp fld = foldDsvFileWithoutHeader comma fp fld

foldCsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
    -> FoldM m (Vector ByteString) a
    -> m (AttoTermination, a)

foldCsvFileWithoutHeaderM fp fld = foldDsvFileWithoutHeaderM comma fp fld
