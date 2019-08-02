{-# LANGUAGE DeriveAnyClass, DerivingStrategies, MultiParamTypeClasses, NoImplicitPrelude #-}

module Dsv.LookupErrors where

import Dsv.IO
import Dsv.Prelude

class DsvError a b where
  dsvError :: a -> b

data RowTooShort = RowTooShort
  deriving stock (Eq, Show)
  deriving anyclass Exception

data DuplicateColumn str = DuplicateColumn str -- ^ Column name
  deriving stock (Eq, Show)
  deriving anyclass Exception

data MissingColumn str = MissingColumn str -- ^ Column name
  deriving stock (Eq, Show)
  deriving anyclass Exception

data InvalidUtf8 str = InvalidUtf8 str -- ^ Column name
  deriving stock (Eq, Show)
  deriving anyclass Exception

data InvalidNat str = InvalidNat str -- ^ Column name
  deriving stock (Eq, Show)
  deriving anyclass Exception
