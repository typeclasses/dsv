{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, NoImplicitPrelude #-}

module Dsv.Position
  ( RowNumber (..)
  , ColumnNumber (..)
  , Position (..)
  ) where

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

data Position =
  Position RowNumber ColumnNumber
  deriving stock (Eq, Ord, Show)
