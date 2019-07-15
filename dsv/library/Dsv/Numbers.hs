{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Dsv.Numbers
  ( fromIntegerMaybe
  ) where

fromIntegerMaybe
    :: forall n. (Bounded n, Integral n)
    => Integer -> Maybe n

fromIntegerMaybe i
    | i < toInteger (minBound @n) = Nothing
    | i > toInteger (maxBound @n) = Nothing
    | otherwise                   = Just (fromInteger @n i)
