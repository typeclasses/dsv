module Dsv.AttoTermination
  ( AttoTermination (..)
  ) where

data AttoTermination
  = AttoComplete
  | AttoIncomplete
  deriving (Eq, Show)
