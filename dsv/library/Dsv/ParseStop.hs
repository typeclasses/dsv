{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.ParseStop
  ( ParseStop (..)
  ) where

import Dsv.Prelude

data ParseStop
  = ParseComplete
  | ParseIncomplete
  deriving (Eq, Show)
