{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.FileFoldCsv
  ( foldCsvFileWithoutHeader, foldCsvFileWithoutHeaderM
  , foldCsvFileIgnoringHeader, foldCsvFileIgnoringHeaderM
  , foldCsvFileWithZippedHeader, foldCsvFileWithZippedHeaderM
  ) where

import DSV.ByteString
import DSV.CommonDelimiters
import DSV.FileFold
import DSV.Fold
import DSV.IO
import DSV.Misc
import DSV.ParseStop
import DSV.Prelude
import DSV.Vector

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
'L.premap' ('fromMaybe' 0 . ('nthVectorElement' 3 >=> 'byteStringDollarsMaybe')) 'L.sum'
@

Result:

@
('ParseComplete', 624.84)
@

-}

foldCsvFileWithoutHeader ::
    forall m result .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldCsvFileWithoutHeader fp fld =
    foldDsvFileWithoutHeader comma fp fld

{- |

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Fold:

@
'L.mapM_' ('traverse_' 'BS.putStrLn' . 'nthVectorElement' 4) *> 'L.generalize' 'L.length'
@

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

@
('ParseComplete', 2)
@

-}

foldCsvFileWithoutHeaderM ::
    forall m result .
    (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

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
'L.premap' ('fromMaybe' 0 . ('nthVectorElement' 3 >=> 'byteStringDollarsMaybe')) 'L.sum'
@

Result:

@
('ParseComplete', 624.84)
@

-}

foldCsvFileIgnoringHeader ::
    forall m result .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

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
'L.mapM_' ('traverse_' 'BS.putStrLn' . 'nthVectorElement' 4) *> 'L.generalize' 'L.length'
@

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

@
('ParseComplete', 2)
@

-}

foldCsvFileIgnoringHeaderM ::
    forall m result .
    (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector ByteString) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

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
'L.premap' ('fromMaybe' 0 . ('vectorLookup' (== \"Price") >=> 'byteStringDollarsMaybe')) 'L.sum'
@

Result:

@
('ParseComplete', 624.84)
@

-}

foldCsvFileWithZippedHeader ::
    forall m result .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Fold (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

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
'L.mapM_' ('traverse_' 'BS.putStrLn' . 'vectorLookup' (== \"Product")) *> 'L.generalize' 'L.length'
@

Output printed to the terminal:

> Dehydrated boulders
> Earthquake pills

Result:

@
('ParseComplete', 2)
@

-}

foldCsvFileWithZippedHeaderM ::
    forall m result .
    (MonadCatch m, MonadMask m, MonadIO m)
    => FilePath
        -- ^ The path of a CSV file to read
    -> FoldM m (Vector (ByteString, ByteString)) result
        -- ^ What to do with each row
    -> m (ParseStop, result)

foldCsvFileWithZippedHeaderM fp fld =
    foldDsvFileWithZippedHeaderM comma fp fld
