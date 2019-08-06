{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.ZipViewPipe
  ( zipViewPipe
  , zipViewPipeIgnoringAllErrors
  , zipViewPipeThrowFirstError
  ) where

import DSV.ByteString
import DSV.IO
import DSV.Position
import DSV.Prelude
import DSV.Validation
import DSV.Vector
import DSV.ViewType
import DSV.ZipViewType

-- pipes
import Pipes
import qualified Pipes.Prelude as P

zipViewPipe ::
    forall m headerError rowError row .
    Monad m
    => ZipView headerError rowError row
    -> Pipe (Vector ByteString) (Validation rowError row) m headerError

zipViewPipe (ZipView (View f)) =
  do
    header <- await
    case (f header) of
        Failure err -> return err
        Success (View g) -> P.map g

zipViewPipeIgnoringAllErrors ::
    forall m headerError rowError row .
    Monad m
    => ZipView headerError rowError row
    -> Pipe (Vector ByteString) row m ()

zipViewPipeIgnoringAllErrors (ZipView (View f)) =
  do
    header <- await
    case (f header) of
        Failure _ -> return ()
        Success (View g) -> P.mapFoldable g

zipViewPipeThrowFirstError ::
    forall m headerError rowError row r .
    ( Monad m, MonadThrow m
    , Exception headerError
    , Show rowError, Typeable rowError
    )
    => ZipView headerError rowError row
    -> Pipe (Vector ByteString) row m r

zipViewPipeThrowFirstError (ZipView (View f)) =
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
