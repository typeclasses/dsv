module Dsv.Atto.Termination (AttoTermination (..)) where

import Dsv.Atto.Error

data AttoTermination
  = AttoComplete
  | AttoIncomplete AttoError
  deriving (Eq, Show)
