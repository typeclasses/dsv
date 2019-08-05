{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.ZipViewStop
  ( ZipViewStop (..)
  ) where

import Dsv.Prelude

-- | A description of what prompted the program to stop reading a DSV file with a header. This is similar to 'ParseTermination', but includes some additional header-specific concerns.

data ZipViewStop headerError =

    ZipViewEmpty
      -- ^ The input contained no rows, not even a header.

  | ZipViewComplete
      -- ^ All of the input was consumed.

  | ZipViewParseError
      -- ^ The parsing stopped where the data was malformed.

  | ZipViewHeaderError headerError
      -- ^ There is some problem with the header that would prevent us from interpreting the subsequent rows.

  deriving (Eq, Show)
