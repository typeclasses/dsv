{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.ParseTermination
  ( ParseTermination (..)
  ) where

import Dsv.Prelude

data ParseTermination
  = ParseComplete
  | ParseIncomplete
  deriving (Eq, Show)
