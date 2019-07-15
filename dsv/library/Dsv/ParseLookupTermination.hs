{-# LANGUAGE BlockArguments, LambdaCase, ScopedTypeVariables #-}

module Dsv.ParseLookupTermination
  ( ParseLookupTermination (..)
  , parseLookupTerminationEitherIso
  ) where

import Dsv.Lens
import Dsv.ParseTermination

-- | Like 'ParseTermination', but with the additional possibility that the reading was stopped by a problem with the header.
data ParseLookupTermination headerError
  = ParseLookupComplete
      -- ^ All of the input was consumed.
  | ParseLookupParseError
      -- ^ The parsing stopped where the data was malformed.
  | ParseLookupHeaderError headerError
      -- ^ There is some problem with the header that would prevent us from interpreting the subsequent rows.

parseLookupTerminationEitherIso ::
  forall x y. Iso
    (ParseLookupTermination  x) (ParseLookupTermination  y)
    (Either ParseTermination x) (Either ParseTermination y)

parseLookupTerminationEitherIso =
  iso
    \case
      ParseLookupComplete       ->  Left ParseComplete
      ParseLookupParseError     ->  Left ParseIncomplete
      ParseLookupHeaderError e  ->  Right e
    \case
      Left ParseComplete        ->  ParseLookupComplete
      Left ParseIncomplete      ->  ParseLookupParseError
      Right e                   ->  ParseLookupHeaderError e
