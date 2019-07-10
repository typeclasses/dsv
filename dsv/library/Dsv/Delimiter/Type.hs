{-# LANGUAGE DeriveLift #-}

module Dsv.Delimiter.Type (Delimiter (..)) where

import Data.Word (Word8)

import Language.Haskell.TH.Syntax (Lift)

newtype Delimiter = Delimiter Word8
    deriving Lift
