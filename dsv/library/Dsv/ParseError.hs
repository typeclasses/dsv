{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.ParseError
  ( ParseError (..)
  ) where

import Dsv.IO
import Dsv.Prelude

data ParseError =
  ParseError
  deriving (Eq, Show)

instance Exception ParseError
  where
    displayException ParseError = "DSV parse error"
