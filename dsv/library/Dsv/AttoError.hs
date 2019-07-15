module Dsv.AttoError
  ( AttoError (..)
  ) where

import Dsv.IO

data AttoError =
  AttoError
  deriving (Eq, Show)

instance Exception AttoError
