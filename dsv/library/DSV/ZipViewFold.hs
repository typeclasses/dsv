{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.ZipViewFold
  ( zipViewFold
  ) where

import DSV.ByteString
import DSV.Fold
import DSV.Validation
import DSV.Vector
import DSV.ViewType
import DSV.ZipViewError
import DSV.ZipViewType

zipViewFold ::
    forall headerError rowError row result .
    ZipView headerError rowError row
    -> Fold (Validation rowError row) result
    -> Fold (Vector ByteString) (Validation (ZipViewError headerError) result)

zipViewFold (ZipView v1) (Fold step init done) = Fold step' Init done'
  where
    step' = \case Init       ->  \x -> case applyView v1 x of Failure e  -> Fail e
                                                              Success v2 -> Go v2 init
                  Fail e     ->  \_ -> Fail e
                  Go v2 acc  ->  \x -> Go v2 (step acc (applyView v2 x))

    done' = \case Init       ->  Failure (ZipViewError_Empty)
                  Fail e     ->  Failure (ZipViewError_HeaderError e)
                  Go _ acc   ->  Success (done acc)

data State headerError rowError row acc =
    Init
  | Fail headerError
  | Go (View rowError (Vector ByteString) row) acc
