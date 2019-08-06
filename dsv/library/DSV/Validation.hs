{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module DSV.Validation
  ( Validation (Success, Failure)
  , overFailure
  ) where

-- validation
import Data.Validation

overFailure :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
overFailure f =
  \case
    Failure e -> Failure (f e)
    Success x -> Success x
