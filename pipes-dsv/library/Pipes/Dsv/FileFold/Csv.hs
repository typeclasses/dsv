module Pipes.Dsv.FileFold.Csv
  ( foldCsvFileWithoutHeader
  , foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader
  , foldCsvFileIgnoringHeaderM
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileFold.Dsv
import Pipes.Dsv.Fold
import Pipes.Dsv.IO
import Pipes.Dsv.Vector

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> L.premap (fromMaybe 0 . ((!? 2) >=> byteStringDollarsMaybe)) L.sum

Result:

> (AttoComplete, 624.84)

-}

foldCsvFileWithoutHeader
    :: MonadIO m
    => FilePath                    -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) a  -- ^ What to do with each row
    -> m (AttoTermination, a)

foldCsvFileWithoutHeader fp fld =
    foldDsvFileWithoutHeader comma fp fld

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> import Data.Foldable (traverse_)
> import Data.Vector ((!?))
> import qualified Data.ByteString.Char8 as BS
> import qualified Control.Foldl as L

> L.mapM_ (traverse_ BS.putStrLn . (!? 3)) *>
> L.generalize L.length

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

> (AttoComplete, 2)

-}

foldCsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath                       -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) a  -- ^ What to do with each row
    -> m (AttoTermination, a)

foldCsvFileWithoutHeaderM fp fld =
    foldDsvFileWithoutHeaderM comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

> L.premap (fromMaybe 0 . ((!? 2) >=> byteStringDollarsMaybe)) L.sum

Result:

> (AttoComplete, 624.84)

-}

foldCsvFileIgnoringHeader
    :: MonadIO m
    => FilePath                       -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) a     -- ^ What to do with each row
    -> m (AttoTermination, a)

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
> import Data.Vector ((!?))
> import qualified Data.ByteString.Char8 as BS
> import qualified Control.Foldl as L

> L.mapM_ (traverse_ BS.putStrLn . (!? 3)) *>
> L.generalize L.length

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

> (AttoComplete, 2)

-}

foldCsvFileIgnoringHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath                       -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) a  -- ^ What to do with each row
    -> m (AttoTermination, a)

foldCsvFileIgnoringHeaderM fp fld =
    foldDsvFileIgnoringHeaderM comma fp fld
