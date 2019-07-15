{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Dsv.AttoLookupTermination
  ( AttoLookupTermination (..)
  , attoLookupTerminationEitherIso
  ) where

import Dsv.AttoError
import Dsv.AttoTermination
import Dsv.Lens

-- | Like 'AttoTermination', but with the additional possibility that the reading was stopped by a problem with the header.
data AttoLookupTermination headerError
  = AttoLookupComplete
      -- ^ All of the input was consumed.
  | AttoLookupParseError AttoError
      -- ^ The parsing stopped where the data was malformed.
  | AttoLookupHeaderError headerError
      -- ^ There is some problem with the header that would prevent us from interpreting the subsequent rows.

attoLookupTerminationEitherIso ::
  forall x y. Iso
    (AttoLookupTermination  x) (AttoLookupTermination  y)
    (Either AttoTermination x) (Either AttoTermination y)

attoLookupTerminationEitherIso = iso f g
  where
    f :: AttoLookupTermination  x -> Either AttoTermination x
    g :: Either AttoTermination y -> AttoLookupTermination  y

    f AttoLookupComplete          =  Left AttoComplete
    f (AttoLookupParseError e)    =  Left (AttoIncomplete e)
    f (AttoLookupHeaderError e)   =  Right e

    g (Left AttoComplete)         =  AttoLookupComplete
    g (Left (AttoIncomplete e))   =  AttoLookupParseError e
    g (Right e)                   =  AttoLookupHeaderError e
