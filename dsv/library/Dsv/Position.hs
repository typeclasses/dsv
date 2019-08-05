{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module Dsv.Position
  ( RowNumber (..)
  , ColumnNumber (..)
  , ColumnName (..)
  , Position (..)
  , At (..)
  , AtHeader (..)
  ) where

import Dsv.IO
import Dsv.Numbers
import Dsv.Prelude

newtype RowNumber =
  RowNumber Positive
  deriving stock (Eq, Ord)
  deriving newtype (Num, Show)

newtype ColumnNumber =
  ColumnNumber Positive
  deriving stock (Eq, Ord)
  deriving newtype (Num, Show)

newtype ColumnName str =
  ColumnName str
  deriving stock (Eq, Ord, Show)

data Position row col =
  Position row col
  deriving stock (Eq, Ord, Show)

data At p a = At p {- ^ Position -} a
  deriving stock (Eq, Ord, Show)
  deriving anyclass Exception

data AtHeader a = AtHeader a
  deriving stock (Eq, Ord, Show)
  deriving anyclass Exception
