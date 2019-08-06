{-# LANGUAGE NoImplicitPrelude #-}

module DSV.ParseStop
  ( ParseStop (..)
  ) where

import DSV.Prelude

data ParseStop
  = ParseComplete
  | ParseIncomplete
  deriving (Eq, Show)
