{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.FileStrictCsvZipView
  ( zipViewCsvFileStrict
  , zipViewCsvFileStrictIgnoringAllErrors
  , zipViewCsvFileStrictThrowFirstError
  ) where

import DSV.CommonDelimiters
import DSV.FileStrictZipView
import DSV.IndexError
import DSV.IO
import DSV.ZipViewStop
import DSV.Position
import DSV.Prelude
import DSV.Text
import DSV.UTF8
import DSV.Validation
import DSV.Vector
import DSV.ZipViews
import DSV.ZipViewType

{- |

=== Example: Reading entire rows

CSV file:

> Date,Vendor,Price,Product
> 2019-03-24,Acme Co,$599.89,Dehydrated boulders
> 2019-04-18,Acme Co,$24.95,Earthquake pills

View:

@
'entireRowZipView' @() @()
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
do
  date    <- 'overZipViewError' (:[]) (:[]) ('textZipViewUtf8' \"Date"    'utf8TextView')
  product <- 'overZipViewError' (:[]) (:[]) ('textZipViewUtf8' \"Product" 'utf8TextView')
  return (date, product)
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
do
  date    <- 'overZipViewError' (:[]) (:[]) ('textZipViewUtf8' \"Date"    'utf8TextView')
  product <- 'overZipViewError' (:[]) (:[]) ('textZipViewUtf8' \"Product" 'utf8TextView')
  return (date, product)
@

Result:

@
( 'ZipViewComplete',
  [ 'Failure' ['At' ('ColumnName' \"Product") ('IndexError_FieldError' 'InvalidUtf8')]
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
