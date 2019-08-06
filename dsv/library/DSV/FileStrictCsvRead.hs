{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.FileStrictCsvRead
  ( readCsvFileStrictWithZippedHeader
  , readCsvFileStrictWithoutHeader
  , readCsvFileStrictIgnoringHeader
  ) where

import DSV.ByteString
import DSV.CommonDelimiters
import DSV.FileStrictRead
import DSV.IO
import DSV.ParseStop
import DSV.Prelude
import DSV.Vector

{- | Often, the first line of a CSV file is a row that gives the name of each column in the file. If present, this row is called the /header/.

=== Example

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> ( ParseComplete,
>   [ [ ("Date",    "2019-03-24"),
>       ("Vendor",  "Acme Co"),
>       ("Price",   "$599.89"),
>       ("Product", "Dehydrated boulders") ],
>     [ ("Date",    "2019-04-18"),
>       ("Vendor",  "Acme Co"),
>       ("Price",   "$24.95"),
>       ("Product", "Earthquake pills") ] ] )

=== Example with a malformed file

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-03-29,Store Mart,"$8.14,Coffee beans
> 2019-04-18,Acme Co,"$24.95,Earthquake pills

Notice the unmatched quotation mark on the third line.

Result:

> ( ParseIncomplete,
>   [ [ ("Date",    "2019-03-24"),
>       ("Vendor",  "Acme Co"),
>       ("Price",   "$599.89"),
>       ("Product", "Dehydrated boulders") ] )

The result includes the first row of data because it appears /before/ the malformed line. All data that comes /after/ the erroneous quotation mark is discarded.

-}

readCsvFileStrictWithZippedHeader ::
    forall m .
    MonadIO m
    => FilePath  -- ^ The path of a CSV file to read
    -> m (ParseStop, (Vector (Vector (ByteString, ByteString))))

readCsvFileStrictWithZippedHeader fp = readDsvFileStrictWithZippedHeader comma fp

{- | Not every CSV file has a header row. Use this function to read a CSV file that does /not/ have a header.

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> ( ParseComplete,
>   [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
>     ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ] )

=== Example with a malformed file

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-03-29,Store Mart,"$8.14,Coffee beans
> 2019-04-18,Acme Co,"$24.95,Earthquake pills

Result:

> ( ParseIncomplete,
>   [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"] ] )

-}
readCsvFileStrictWithoutHeader ::
    forall m .
    MonadIO m
    => FilePath  -- ^ The path of a CSV file to read
    -> m (ParseStop, (Vector (Vector ByteString)))

readCsvFileStrictWithoutHeader fp = readDsvFileStrictWithoutHeader comma fp

{- | Sometimes a CSV file has a header but you don't care about it. In that case, you can use this function to /ignore/ the header line and read only the rows containing data.

=== Example

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> ( ParseComplete,
>   [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
>     ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ] )

-}

readCsvFileStrictIgnoringHeader ::
    forall m .
    MonadIO m
    => FilePath  -- ^ The path of a CSV file to read
    -> m (ParseStop, (Vector (Vector ByteString)))

readCsvFileStrictIgnoringHeader fp = readDsvFileStrictIgnoringHeader comma fp
