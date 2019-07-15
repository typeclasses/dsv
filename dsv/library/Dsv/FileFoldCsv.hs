{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Dsv.FileFoldCsv
  ( foldCsvFileWithoutHeader, foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader, foldCsvFileIgnoringHeaderM
  , foldCsvFileWithZippedHeader, foldCsvFileWithZippedHeaderM
  ) where

import Dsv.ByteString
import Dsv.CommonDelimiters
import Dsv.FileFold
import Dsv.Fold
import Dsv.IO
import Dsv.Misc
import Dsv.ParseTermination
import Dsv.Vector

-- base
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

-- bytestring
import qualified Data.ByteString.Char8 as BS

-- foldl
import qualified Control.Foldl as L

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.premap' ('fromMaybe' 0 . ('nthColumn' 3 >=> 'byteStringDollarsMaybe')) 'L.sum'
@

Result:

@
('ParseComplete', 624.84)
@

-}

foldCsvFileWithoutHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseTermination, result)

foldCsvFileWithoutHeader fp fld =
    foldDsvFileWithoutHeader comma fp fld

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.mapM_' ('traverse_' 'BS.putStrLn' . 'nthColumn' 4) *> 'L.generalize' 'L.length'
@

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

@
('ParseComplete', 2)
@

-}

foldCsvFileWithoutHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseTermination, result)

foldCsvFileWithoutHeaderM fp fld =
    foldDsvFileWithoutHeaderM comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.premap' ('fromMaybe' 0 . ('nthColumn' 3 >=> 'byteStringDollarsMaybe')) 'L.sum'
@

Result:

@
('ParseComplete', 624.84)
@

-}

foldCsvFileIgnoringHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseTermination, result)

foldCsvFileIgnoringHeader fp fld =
    foldDsvFileIgnoringHeader comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.mapM_' ('traverse_' 'BS.putStrLn' . 'nthColumn' 4) *> 'L.generalize' 'L.length'
@

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

@
('ParseComplete', 2)
@

-}

foldCsvFileIgnoringHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseTermination, result)

foldCsvFileIgnoringHeaderM fp fld =
    foldDsvFileIgnoringHeaderM comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.premap' ('fromMaybe' 0 . ('columnName' "Price" >=> 'byteStringDollarsMaybe')) 'L.sum'
@

Result:

@
('ParseComplete', 624.84)
@

-}

foldCsvFileWithZippedHeader
    :: MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (ParseTermination, result)

foldCsvFileWithZippedHeader fp fld =
    foldDsvFileWithZippedHeader comma fp fld

{- |

=== Example

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.mapM_' ('traverse_' 'BS.putStrLn' . 'columnName' "Product") *> 'L.generalize' 'L.length'
@

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

@
('ParseComplete', 2)
@

-}

foldCsvFileWithZippedHeaderM
    :: (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (ParseTermination, result)

foldCsvFileWithZippedHeaderM fp fld =
    foldDsvFileWithZippedHeaderM comma fp fld
