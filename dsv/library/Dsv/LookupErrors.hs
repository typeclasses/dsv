{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

module Dsv.LookupErrors
  ( Missing (..)
  , Duplicate (..)
  , LookupError (..)
  , TooShort (..)
  , IndexError (..)
  ) where

import Dsv.IO
import Dsv.Prelude

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
