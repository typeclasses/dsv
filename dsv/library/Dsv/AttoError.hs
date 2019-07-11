module Dsv.AttoError
  ( AttoError (..)
  ) where

import Dsv.IO

data AttoError =
  AttoError
    [String] -- ^ A list of contexts in which the error occurred.
    String -- ^ The message describing the error, if any.
  deriving (Eq, Show)

instance Exception AttoError
