module Pipes.Dsv.Atto.Termination (AttoTermination (..)) where

import Pipes.Dsv.Atto.Error

data AttoTermination
  = AttoComplete
  | AttoIncomplete AttoError
  deriving (Eq, Show)
