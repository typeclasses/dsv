{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

module DSV.LookupErrorUtf8
  ( LookupErrorUtf8 (..)
  ) where

import DSV.IO
import DSV.Prelude

-- | The general concept of what can go wrong when you look up the position of a particular element in a list.
data LookupErrorUtf8
  = LookupErrorUtf8_Missing  -- ^ There is /no/ matching element.
  | LookupErrorUtf8_Duplicate  -- ^ There are /more than one/ matching elements.
  | LookupErrorUtf8_Invalid  -- ^ Found one matching element, but it is not a valid UTF-8 string.
  deriving stock (Eq, Show)
  deriving anyclass Exception
