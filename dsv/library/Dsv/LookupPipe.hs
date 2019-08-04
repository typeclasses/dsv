{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Dsv.LookupPipe
  ( lookupPipe
  , lookupPipeIgnoringAllErrors
  , lookupPipeThrowFirstError
  ) where

import Dsv.ByteString
import Dsv.IO
import Dsv.LookupType
import Dsv.Prelude
import Dsv.Validation
import Dsv.Vector
import Dsv.ViewType

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
    (Monad m, MonadThrow m, Exception headerError, Exception rowError)
    => Lookup headerError rowError row
    -> Pipe (Vector ByteString) row m r

lookupPipeThrowFirstError (Lookup (View f)) =
  do
    header <- await
    case (f header) of
        Failure e -> throwM e
        Success (View g) ->
            P.mapM $ \x ->
                case g x of
                    Failure e -> throwM e
                    Success row -> return row
