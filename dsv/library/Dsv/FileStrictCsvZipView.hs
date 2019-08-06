{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.FileStrictCsvZipView
  ( zipViewCsvFileStrict
  , zipViewCsvFileStrictIgnoringAllErrors
  , zipViewCsvFileStrictThrowFirstError
  ) where

import Dsv.CommonDelimiters
import Dsv.English
import Dsv.FileStrictZipView
import Dsv.IO
import Dsv.ZipViewStop
import Dsv.Prelude
import Dsv.Text
import Dsv.Validation
import Dsv.Vector
import Dsv.ZipViews
import Dsv.ZipViewType

{- |

=== Example: Reading entire rows

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

View:

@
'entireRowZipView'
@

Result:

@
( 'ZipViewComplete',
  [ 'Success' ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"],
    'Success' ["2019-04-18", "Acme Co", "$24.95",  "Earthquake pills"] ] )
@

=== Example: Reading particular columns

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

View:

@
((,) \<$> 'textZipViewUtf8' \"Date" \<*> 'textZipViewUtf8' \"Product")
    :: 'ZipView' 'EnglishText' 'EnglishText' ('Text', 'Text')
@

Result:

@
( 'ZipViewComplete',
  [ 'Success' ("2019-03-24", "Dehydrated boulders"),
    'Success' ("2019-04-18", "Earthquake pills") ] )
@

=== Example: Decoding errors

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydra\xc3\x28d boulders
> 2019-04-18,\xc3\x28me Co,$24.95,Earthquake pills

In this example, @\\xc3\\x28@ represents two bytes which constitute an invalid sequence in UTF-8. Notice that there is a UTF-8 error on each of the last two lines.

View:

@
((,) \<$> 'textZipViewUtf8' \"Date" \<*> 'textZipViewUtf8' \"Product")
    :: 'ZipView' 'EnglishText' 'EnglishText' ('Text', 'Text')
@

Result:

@
( 'ZipViewComplete',
  [ 'Failure' ('EnglishText' ["The byte string in column named \'Product' is not valid UTF-8."])
  , 'Success' ("2019-04-18", "Earthquake pills")
  ]
@

The first item in the result is a 'Failure', because we tried to decode the value in the \"Product" column, and it cannot be decoded as UTF-8. The second item in the result is a 'Success', because although the row does contain an encoding error, the error is in the \"Vendor" field, which we never read.

-}

zipViewCsvFileStrict ::
    forall m headerError rowError row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (ZipViewStop headerError, Vector (Validation rowError row))

zipViewCsvFileStrict fp lu =
    zipViewDsvFileStrict comma fp lu

zipViewCsvFileStrictIgnoringAllErrors ::
    forall m headerError rowError row .
    MonadIO m
    => FilePath
        -- ^ The path of a CSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

zipViewCsvFileStrictIgnoringAllErrors fp lu =
    zipViewDsvFileStrictIgnoringAllErrors comma fp lu

zipViewCsvFileStrictThrowFirstError ::
    forall m headerError rowError row .
    ( MonadIO m
    , Exception headerError
    , Show rowError, Typeable rowError
    )
    => FilePath
        -- ^ The path of a CSV file to read
    -> ZipView headerError rowError row
        -- ^ How to interpret the rows
    -> m (Vector row)

zipViewCsvFileStrictThrowFirstError fp lu =
    zipViewDsvFileStrictThrowFirstError comma fp lu
