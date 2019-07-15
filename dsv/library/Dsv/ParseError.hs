module Dsv.ParseError
  ( ParseError (..)
  ) where

import Dsv.IO

data ParseError =
  ParseError
  deriving (Eq, Show)

instance Exception ParseError
  where
    displayException ParseError = "DSV parse error"
