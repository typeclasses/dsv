{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.ZipViewPipe
  ( zipViewPipe
  , zipViewPipeIgnoringAllErrors
  , zipViewPipeThrowFirstError
  ) where

import DSV.ByteString
import DSV.IO
import DSV.Pipes
import DSV.Position
import DSV.Prelude
import DSV.Validation
import DSV.Vector
import DSV.ViewType
import DSV.ZipViewType

-- pipes
import qualified Pipes.Prelude as P

zipViewPipe ::
    forall m headerError rowError row .
    Monad m
    => ZipView headerError rowError row
      -- ^ A specification of how to interpret the header and rows
    -> Pipe (Vector ByteString) (Validation rowError row) m headerError
      -- ^ The first vector that this pipe 'await's is the header. If the header is invalid, the pipe closes and 'return's the @headerError@. Otherwise, the pipe continues indefinitely; for each subsequent @'Vector' 'ByteString'@, it 'yield's one @'Validation' rowError row@.

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
      -- ^ A specification of how to interpret the header and rows
    -> Pipe (Vector ByteString) row m ()
      -- ^ The first vector that this pipe 'await's is the header. If the header is invalid, the pipe closes and 'return's @()@. Otherwise, the pipe continues indefinitely; for each subsequent @'Vector' 'ByteString'@, it 'yield's a @row@ if the row is valid, or otherwise does nothing if the row is malformed.

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
      -- ^ A specification of how to interpret the header and rows
    -> Pipe (Vector ByteString) row m r
      -- ^ The first vector that this pipe 'await's is the header. If the header is invalid, the pipe throws the @headerError@ as an exception in @m@. For each subsequent @'Vector' 'ByteString'@, the pipe 'yield's a @row@ if the row is valid, or otherwise throws the @rowError@ as an exception in @m@.

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
