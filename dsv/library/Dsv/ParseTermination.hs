module Dsv.ParseTermination
  ( ParseTermination (..)
  ) where

data ParseTermination
  = ParseComplete
  | ParseIncomplete
  deriving (Eq, Show)
