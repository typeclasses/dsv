module DSV.TestPrelude
  ( module Prelude
  , example
  ) where

import Prelude hiding (product)
import DSV as Prelude
import Paths_dsv as Prelude
import Control.Monad.IO.Class as Prelude
import Data.Bifunctor as Prelude
import Data.IORef as Prelude
import Hedgehog as Prelude

import Control.Monad as Prelude
    ((>=>))

import Data.Foldable as Prelude
    (toList, traverse_)

import Data.Maybe as Prelude
    (fromMaybe)

example =
    withTests 1 . property
