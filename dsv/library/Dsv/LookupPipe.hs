{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dsv.LookupPipe
  ( lookupPipe
  , lookupPipeIgnoringAllErrors
  , lookupPipeThrowFirstError
  ) where

import Dsv.ByteString
import Dsv.IO
import Dsv.Position
import Dsv.Prelude
import Dsv.Validation
import Dsv.Vector
import Dsv.ViewType
import Dsv.ZipViewType

-- pipes
import Pipes
import qualified Pipes.Prelude as P

lookupPipe ::
    forall m headerError rowError row .
    Monad m
    => Lookup headerError rowError row
    -> Pipe (Vector ByteString) (Validation rowError row) m headerError

lookupPipe (Lookup (View f)) =
  do
    header <- await
    case (f header) of
        Failure err -> return err
        Success (View g) -> P.map g

lookupPipeIgnoringAllErrors ::
    forall m headerError rowError row .
    Monad m
    => Lookup headerError rowError row
    -> Pipe (Vector ByteString) row m ()

lookupPipeIgnoringAllErrors (Lookup (View f)) =
  do
    header <- await
    case (f header) of
        Failure _ -> return ()
        Success (View g) -> P.mapFoldable g

lookupPipeThrowFirstError ::
    forall m headerError rowError row r .
    ( Monad m, MonadThrow m
    , Exception headerError
    , Show rowError, Typeable rowError
    )
    => Lookup headerError rowError row
    -> Pipe (Vector ByteString) row m r

lookupPipeThrowFirstError (Lookup (View f)) =
  do
    header <- await
    case (f header) of
        Failure e -> throwM e
        Success v -> go v (RowNumber 1)
  where
    go v (RowNumber n) =
      do
        x <- await
        case applyView v x of
            Failure e -> throwM (At (RowNumber n) e)
            Success row ->
              do
                yield row
                go v (RowNumber (n + 1))
