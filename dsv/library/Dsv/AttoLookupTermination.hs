{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Dsv.AttoLookupTermination
  ( AttoLookupTermination (..)
  , attoLookupTerminationEitherIso
  ) where

import Dsv.Lens
import Dsv.ParseTermination

-- | Like 'ParseTermination', but with the additional possibility that the reading was stopped by a problem with the header.
data AttoLookupTermination headerError
  = AttoLookupComplete
      -- ^ All of the input was consumed.
  | AttoLookupParseError
      -- ^ The parsing stopped where the data was malformed.
  | AttoLookupHeaderError headerError
      -- ^ There is some problem with the header that would prevent us from interpreting the subsequent rows.

attoLookupTerminationEitherIso ::
  forall x y. Iso
    (AttoLookupTermination  x) (AttoLookupTermination  y)
    (Either ParseTermination x) (Either ParseTermination y)

attoLookupTerminationEitherIso = iso f g
  where
    f :: AttoLookupTermination   x -> Either ParseTermination x
    g :: Either ParseTermination y -> AttoLookupTermination  y

    f AttoLookupComplete          =  Left ParseComplete
    f AttoLookupParseError        =  Left ParseIncomplete
    f (AttoLookupHeaderError e)   =  Right e

    g (Left ParseComplete)        =  AttoLookupComplete
    g (Left ParseIncomplete)      =  AttoLookupParseError
    g (Right e)                   =  AttoLookupHeaderError e
