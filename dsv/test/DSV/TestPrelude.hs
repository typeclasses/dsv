module DSV.TestPrelude
  ( module Prelude
  , example
  ) where

import Prelude hiding (product)
import DSV as Prelude
import Paths_dsv as Prelude
import Control.Monad.IO.Class as Prelude
import Data.Bifunctor as Prelude
import Hedgehog as Prelude

example =
    withTests 1 . property
