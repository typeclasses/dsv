module Pipes.Dsv.FileFold.Csv
  ( foldCsvFileWithoutHeader
  , foldCsvFileWithoutHeaderM
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileFold.Dsv
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

-- foldl
import Control.Foldl (Fold, FoldM)

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
