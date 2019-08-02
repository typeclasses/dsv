{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.ParseLookupTermination
  ( ParseLookupTermination (..)
  ) where

import Dsv.Prelude

-- | A description of what prompted the program to stop reading a DSV file with a header. This is similar to 'ParseTermination', but includes some additional header-specific concerns.

data ParseLookupTermination headerError =

    ParseLookupEmpty
      -- ^ The input contained no rows, not even a header.

  | ParseLookupComplete
      -- ^ All of the input was consumed.

  | ParseLookupParseError
      -- ^ The parsing stopped where the data was malformed.

  | ParseLookupHeaderError headerError
      -- ^ There is some problem with the header that would prevent us from interpreting the subsequent rows.

  deriving (Eq, Show)
