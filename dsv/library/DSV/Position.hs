{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module DSV.Position
  ( RowNumber (..)
  , ColumnNumber (..)
  , ColumnName (..)
  , Position (..)
  , At (..)
  , AtHeader (..)
  ) where

import DSV.IO
import DSV.Numbers
import DSV.Prelude

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
