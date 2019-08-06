{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

module DSV.LookupError
  ( Missing (..)
  , Duplicate (..)
  , LookupError (..)
  ) where

import DSV.IO
import DSV.Prelude

data Missing = Missing
  deriving stock (Eq, Show)
  deriving anyclass Exception

data Duplicate = Duplicate
  deriving stock (Eq, Show)
  deriving anyclass Exception

-- | The general concept of what can go wrong when you look up the position of a particular element in a list.
data LookupError
  = LookupError_Missing  -- ^ There is /no/ matching element.
  | LookupError_Duplicate  -- ^ There are /more than one/ matching elements.
  deriving stock (Eq, Show)
  deriving anyclass Exception
