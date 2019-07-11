module Dsv.AttoTermination
  ( AttoTermination (..)
  ) where

import Dsv.AttoError

data AttoTermination
  = AttoComplete
  | AttoIncomplete AttoError
  deriving (Eq, Show)
