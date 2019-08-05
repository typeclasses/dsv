{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.Prelude (module Prelude) where

import Prelude hiding ((.), id)
import Control.Category as Prelude ((.), id)
import Data.Coerce as Prelude
import Data.Functor.Compose as Prelude (Compose (Compose))
import Data.Typeable as Prelude (Typeable)
