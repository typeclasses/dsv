{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileStrictCsvLookup
  ( lookupCsvFileStrict
  , lookupCsvFileStrictIgnoringAllErrors
  , lookupCsvFileStrictThrowFirstError
  ) where

import Dsv.CommonDelimiters
import Dsv.English
import Dsv.FileStrictLookup
import Dsv.IO
import Dsv.Lookups
import Dsv.LookupType
import Dsv.ParseLookupTermination
import Dsv.Text
import Dsv.Validation
import Dsv.Vector

{- |

=== Example: Reading entire rows

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Lookup:

@
'entireRowLookup'
@

Result:

@
( 'ParseLookupComplete',
  [ 'Success' ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
    'Success' ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ] )
@

=== Example: Reading particular columns

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

Lookup:

@
((,) \<$> 'textLookupUtf8' \"Date" \<*> 'textLookupUtf8' \"Product")
    :: 'Lookup' 'EnglishText' 'EnglishText' ('Text', 'Text')
@

Result:

@
( 'ParseLookupComplete',
  [ 'Success' ("2019-03-24", "Dehydrated boulders"),
    'Success' ("2019-04-18", "Earthquake pills") ] )
@

=== Example: Decoding errors

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydra\xc3\x28d boulders
> 2019-04-18,\xc3\x28me Co,$24.95,Earthquake pills

In this example, @\\xc3\\x28@ represents two bytes which constitute an invalid sequence in UTF-8. Notice that there is a UTF-8 error on each of the last two lines.

Lookup:

@
((,) \<$> 'textLookupUtf8' \"Date" \<*> 'textLookupUtf8' \"Product")
    :: 'Lookup' 'EnglishText' 'EnglishText' ('Text', 'Text')
@

Result:

@
( 'ParseLookupComplete',
  [ 'Failure' ('EnglishText' ["The byte string in column named \'Product' is not valid UTF-8."])
  , 'Success' ("2019-04-18", "Earthquake pills")
  ]
@

The first item in the result is a 'Failure', because we tried to decode the value in the \"Product" column, and it cannot be decoded as UTF-8. The second item in the result is a 'Success', because although the row does contain an encoding error, the error is in the \"Vendor" field, which we never read.

-}

lookupCsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Lookup headerError rowError row
        -- ^ How to interpret the rows
    -> m (ParseLookupTermination headerError, Vector (Validation rowError row))

lookupCsvFileStrict fp lu =
    lookupDsvFileStrict comma fp lu

lookupCsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> Lookup headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

lookupCsvFileStrictIgnoringAllErrors fp lu =
    lookupDsvFileStrictIgnoringAllErrors comma fp lu

lookupCsvFileStrictThrowFirstError ::
    forall m headerError rowError row .
    (MonadIO m, Exception headerError, Exception rowError)
    => FilePath
        -- ^ The path of a CSV file to read
    -> Lookup headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

lookupCsvFileStrictThrowFirstError fp lu =
    lookupDsvFileStrictThrowFirstError comma fp lu
