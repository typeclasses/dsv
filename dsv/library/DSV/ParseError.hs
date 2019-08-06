{-# LANGUAGE NoImplicitPrelude #-}

module DSV.ParseError
  ( ParseError (..)
  ) where

import DSV.IO
import DSV.Prelude

data ParseError =
  ParseError
  deriving (Eq, Show)

instance Exception ParseError
  where
    displayException ParseError = "DSV parse error"
