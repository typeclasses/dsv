{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies, DeriveLift #-}

module DSV.DelimiterType
  ( Delimiter (..)
  , delimiterWord8
  ) where

import Data.Word (Word8)

import Language.Haskell.TH.Syntax (Lift)

newtype Delimiter = Delimiter Word8
    deriving stock Lift

delimiterWord8 :: Delimiter -> Word8
delimiterWord8 (Delimiter d) = d
