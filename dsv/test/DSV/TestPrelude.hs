{-# LANGUAGE NoImplicitPrelude #-}

module DSV.TestPrelude
  ( module Prelude
  , example
  ) where

import DSV as Prelude

import Paths_dsv as Prelude
import Prelude hiding (product)

import Control.Monad.IO.Class as Prelude
import Data.Bifunctor as Prelude
import Data.IORef as Prelude
import Hedgehog as Prelude

import Control.Category as Prelude
    ((>>>), (<<<))

import Control.Exception.Safe as Prelude
    (try)

import Control.Monad as Prelude
    ((>=>))

import Data.Foldable as Prelude
    (toList, traverse_)

import Data.Maybe as Prelude
    (fromMaybe)

import Data.Monoid as Prelude
    (Sum (..))

import Data.Semigroup as Prelude
    (First (..))

example =
    withTests 1 . property
