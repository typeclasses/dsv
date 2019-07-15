module Dsv.LookupType
  ( Lookup (..)
  ) where

import Dsv.ByteString
import Dsv.Validation
import Dsv.Vector

-- base
import Control.Applicative

newtype Lookup he re a =
  Lookup (Vector ByteString -> Validation he (Vector ByteString -> Validation re a))

instance Functor (Lookup he re)
  where
    fmap f (Lookup x) = Lookup ((fmap . fmap . fmap . fmap) f x)

instance (Semigroup he, Semigroup re) => Applicative (Lookup he re)
  where
    pure = Lookup . (pure . pure . pure . pure)
    liftA2 f (Lookup x1) (Lookup x2) =
      Lookup
        (\h ->
            liftA2
                (\y1 y2 r -> liftA2 f (y1 r) (y2 r))
                (x1 h) (x2 h)
        )
