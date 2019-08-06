{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSV.VectorViews
  ( columnNumberView
  , lookupView
  ) where

import DSV.IndexError
import DSV.LookupError
import DSV.Numbers
import DSV.Position
import DSV.Prelude
import DSV.Validation
import DSV.Vector
import DSV.ViewType

columnNumberView :: forall a.
    ColumnNumber -> View TooShort (Vector a) a

columnNumberView (ColumnNumber n) =
    View $ \xs ->
        case vectorIndexInt xs (positiveInt n - 1) of
            Nothing -> Failure TooShort
            Just x  -> Success x

lookupView ::
    (a -> Bool)
    -> View LookupError (Vector (a, b)) b

lookupView f =
    View $ \xs ->
        case filter (\(n, _) -> f n) (toList xs) of
            [] -> Failure LookupError_Missing
            [(_, v)] -> Success v
            _ -> Failure LookupError_Duplicate
