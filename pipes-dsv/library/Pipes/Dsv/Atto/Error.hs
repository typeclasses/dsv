module Pipes.Dsv.Atto.Error (AttoError (..)) where

import Pipes.Dsv.IO

data AttoError =
  AttoError
    [String] -- ^ A list of contexts in which the error occurred.
    String -- ^ The message describing the error, if any.
  deriving (Eq, Show)

instance Exception AttoError
