{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies, DeriveFunctor, DerivingVia, StandaloneDeriving #-}

module DSV.ZipViewType
  ( ZipView (..), refineZipView
  , overHeaderError, overRowError, overZipViewError
  ) where

import DSV.ByteString
import DSV.Prelude
import DSV.Validation
import DSV.ViewType
import DSV.Vector

-- base
import Data.Functor.Compose (Compose (Compose))

{- |

'ZipView' captures a common pattern for consuming a DSV file with a header row: First we have one 'View' that looks at the header row, and from that we determine how to view the subsequent rows of data. We use that second 'View' to interpret each row.

For example, if we want to read the \"Date" and \"Price" columns, when we read the header we may see that these are the first and third columns, respectively; and so the first 'View' will return a 'View' that reads the first and third column of each row.

=== Errors

There are two distinct modes of failure in this process, represented by the two type parameters @headerError@ and @rowError@.

- A 'Failure' of the @headerError@ type is produced by the first 'View' if the header is malformed in a way that prevents us from being able to read the data rows - for example, if we want to read the \"Date" column but the header does not contain any entry with that name.

- A 'Failure' of the @rowError@ type is produced by the second 'View' for each malformed row - for example, if \"Price" is the third column but the row only contains two entries, or if we require the entry to contain a dollar amount but it contains some other unrecognizable string.

Note that header errors which are unrecoverable, whereas it is possible to continue past row errors and get a mixture of 'Failure' and 'Success' results among the rows.

-}

newtype ZipView headerError rowError a =
  ZipView
    (View headerError (Vector ByteString)
      (View rowError (Vector ByteString) a))
  deriving stock Functor

-- | 'ZipView' has an 'Applicative' but no 'Monad', so you may wish to enable the @ApplicativeDo@ GHC extension.

deriving via
  Compose
    (View headerError (Vector ByteString))
    (View rowError (Vector ByteString))
  instance
    (Semigroup headerError, Semigroup rowError) =>
    Applicative (ZipView headerError rowError)

refineZipView ::
    forall headerError rowError a b .
    ZipView headerError rowError a
      -- ^ A view that produces a value of type @a@ for each row.
    -> View rowError a b
      -- ^ A way to interpret that @a@ value as a different type @b@.
    -> ZipView headerError rowError b
      -- ^ A view that produces a value of type @b@ for each row.

refineZipView (ZipView (View f)) r2 =
  ZipView $
    View $
      fmap (r2 .) . f

overHeaderError ::
    (headerError1 -> headerError2) ->
    ZipView headerError1 rowError a ->
    ZipView headerError2 rowError a

overHeaderError f (ZipView v) =
    ZipView (overViewError f v)

overRowError ::
    (rowError1 -> rowError2) ->
    ZipView headerError rowError1 a ->
    ZipView headerError rowError2 a

overRowError f (ZipView v) =
    ZipView (fmap (overViewError f) v)

overZipViewError ::
    forall headerError1 headerError2 rowError1 rowError2 a .
    (headerError1 -> headerError2) -> (rowError1 -> rowError2)
    -> ZipView headerError1 rowError1 a
    -> ZipView headerError2 rowError2 a

overZipViewError f g =
    overRowError g .
    overHeaderError f
