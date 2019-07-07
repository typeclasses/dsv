module Pipes.Dsv.CsvFileStrict
  ( readCsvFileStrictUsingHeader
  , readCsvFileStrictWithoutHeader
  , readCsvFileStrictIgnoringHeader
  ) where

import Pipes.Dsv.Atto
import Pipes.Dsv.ByteString
import Pipes.Dsv.Delimiter
import Pipes.Dsv.FileStrict
import Pipes.Dsv.Header
import Pipes.Dsv.Vector

{- | Often, the first line of a CSV file is a row that gives the name of each column in the file. If present, this row is called the /header/.

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> [ [ Labeled "Date"   "2019-03-24",
>     Labeled "Vendor" "Acme Co",
>     Labeled "Price"  "$599.89",
>     Labeled "Notes"  "Dehydrated boulders" ],
>   [ Labeled "Date"   "2019-04-18",
>     Labeled "Vendor" "Acme Co",
>     Labeled "Price"  "$24.95",
>     Labeled "Notes"  "Earthquake pills" ] ]

-}

readCsvFileStrictUsingHeader :: FilePath -> IO (AttoTermination, [Vector (Labeled ByteString ByteString)])
readCsvFileStrictUsingHeader fp = readDsvFileStrictUsingHeader comma fp

{- | Not every CSV file has a header row. Use this function to read a CSV file that does /not/ have a header.

=== Example

CSV file:

> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
>   ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ]

-}
readCsvFileStrictWithoutHeader :: FilePath -> IO (AttoTermination, [Vector ByteString])
readCsvFileStrictWithoutHeader fp = readDsvFileStrictWithoutHeader comma fp

{- | Sometimes a CSV file has a header but you don't care about it. In that case, you can use this function to /ignore/ the header line and read only the rows containing data.

=== Example

CSV file:

> Date,Vendor,Price,Notes
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Result:

> [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
>   ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ]

-}

readCsvFileStrictIgnoringHeader :: FilePath -> IO (AttoTermination, [Vector ByteString])
readCsvFileStrictIgnoringHeader fp = readDsvFileStrictIgnoringHeader comma fp
