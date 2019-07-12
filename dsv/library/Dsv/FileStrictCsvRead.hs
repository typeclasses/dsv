module Dsv.FileStrictCsvRead
  ( readCsvFileStrictWithZippedHeader
  , readCsvFileStrictWithoutHeader
  , readCsvFileStrictIgnoringHeader
  ) where

import Dsv.AttoTermination
import Dsv.ByteString
import Dsv.Delimiter
import Dsv.FileStrictRead
import Dsv.IO
import Dsv.Vector

{- | Often, the first line of a CSV file is a row that gives the name of each column in the file. If present, this row is called the /header/.

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> ( AttoComplete,
>   [ [ ("Date",   "2019-03-24"),
>       ("Vendor", "Acme Co"),
>       ("Price",  "$599.89"),
>       ("Notes",  "Dehydrated boulders") ],
>     [ ("Date",   "2019-04-18"),
>       ("Vendor", "Acme Co"),
>       ("Price",  "$24.95"),
>       ("Notes",  "Earthquake pills") ] ] )

-}

readCsvFileStrictWithZippedHeader
    :: MonadIO m
    => FilePath  -- ^ The path of a CSV file to read
    -> m (AttoTermination, (Vector (Vector (ByteString, ByteString))))

readCsvFileStrictWithZippedHeader fp = readDsvFileStrictWithZippedHeader comma fp

{- | Not every CSV file has a header row. Use this function to read a CSV file that does /not/ have a header.

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> ( AttoComplete,
>   [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
>     ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ] )

-}
readCsvFileStrictWithoutHeader
    :: MonadIO m
    => FilePath  -- ^ The path of a CSV file to read
    -> m (AttoTermination, (Vector (Vector ByteString)))

readCsvFileStrictWithoutHeader fp = readDsvFileStrictWithoutHeader comma fp

{- | Sometimes a CSV file has a header but you don't care about it. In that case, you can use this function to /ignore/ the header line and read only the rows containing data.

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> ( AttoComplete,
>   [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
>     ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ] )

-}

readCsvFileStrictIgnoringHeader
    :: MonadIO m
    => FilePath  -- ^ The path of a CSV file to read
    -> m (AttoTermination, (Vector (Vector ByteString)))

readCsvFileStrictIgnoringHeader fp = readDsvFileStrictIgnoringHeader comma fp
