module Pipes.Dsv.FileFold.Csv
  ( foldCsvFileWithoutHeader, foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader, foldCsvFileIgnoringHeaderM
  , foldCsvFileUsingHeader, foldCsvFileUsingHeaderM
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileFold.Dsv
import Pipes.Dsv.Fold
import Pipes.Dsv.Header
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> L.premap (fromMaybe 0 . (nthColumn 3 >=> byteStringDollarsMaybe)) L.sum

Result:

> (AttoComplete, 624.84)

-}

foldCsvFileWithoutHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldCsvFileWithoutHeader fp fld =
    foldDsvFileWithoutHeader comma fp fld

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> import Data.Foldable (traverse_)
> import qualified Data.ByteString.Char8 as BS
> import qualified Control.Foldl as L

> L.mapM_ (traverse_ BS.putStrLn . nthColumn 4) *>
> L.generalize L.length

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

> (AttoComplete, 2)

-}

foldCsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldCsvFileWithoutHeaderM fp fld =
    foldDsvFileWithoutHeaderM comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> L.premap (fromMaybe 0 . (nthColumn 3 >=> byteStringDollarsMaybe)) L.sum

Result:

> (AttoComplete, 624.84)

-}

foldCsvFileIgnoringHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldCsvFileIgnoringHeader fp fld =
    foldDsvFileIgnoringHeader comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> import Data.Foldable (traverse_)
> import qualified Data.ByteString.Char8 as BS
> import qualified Control.Foldl as L

> L.mapM_ (traverse_ BS.putStrLn . nthColumn 4) *>
> L.generalize L.length

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

> (AttoComplete, 2)

-}

foldCsvFileIgnoringHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldCsvFileIgnoringHeaderM fp fld =
    foldDsvFileIgnoringHeaderM comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> L.premap (fromMaybe 0 . (columnName "Price" >=> byteStringDollarsMaybe)) L.sum

Result:

> (AttoComplete, 624.84)

-}

foldCsvFileUsingHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector (Labeled ByteString ByteString)) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldCsvFileUsingHeader fp fld =
    foldDsvFileUsingHeader comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> import Data.Foldable (traverse_)
> import qualified Data.ByteString.Char8 as BS
> import qualified Control.Foldl as L

> L.mapM_ (traverse_ BS.putStrLn . columnName "Notes") *>
> L.generalize L.length

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

> (AttoComplete, 2)

-}

foldCsvFileUsingHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector (Labeled ByteString ByteString)) result
        -- ^ What to do with each row
    -> m (AttoTermination, result)

foldCsvFileUsingHeaderM fp fld =
    foldDsvFileUsingHeaderM comma fp fld
