{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

module Dsv.IndexError
  ( TooShort (..)
  , IndexError (..)
  ) where

import Dsv.IO
import Dsv.Prelude

data TooShort = TooShort
  deriving stock (Eq, Show)
  deriving anyclass Exception

-- | The general concept of what can go wrong when you look up something by position in a list.
data IndexError error
  = IndexError_TooShort
      -- ^ There is no element at that position because the list isn't long enough.
  | IndexError_FieldError error
      -- ^ There is something wrong with the element found at the position.
  deriving stock (Eq, Show)
  deriving anyclass Exception
