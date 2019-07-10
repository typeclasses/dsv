module Pipes.Dsv.Delimiter.TH (charDelimiter) where

import Pipes.Dsv.Delimiter.Type (Delimiter (..))

import Data.Word (Word8)

import qualified Data.Char

import Language.Haskell.TH.Syntax (Q, Exp, lift)

-- | A Template Haskell expression of type 'Delimiter'. Rejects code points above @0xff@.
--
-- === Example
--
-- 'comma' is defined as:
--
-- @$('charDelimiter' ',')@
--
-- This could be written equivalently as:
--
-- @'Delimiter' ('fromIntegral' ('Data.Char.ord' ','))@
--
-- but the former includes a compile-time check to ensure that the character @','@ is representable by a single byte (and thus that 'fromIntegral' does not overflow).

charDelimiter :: Char -> Q Exp
charDelimiter c =
  do
    let n = Data.Char.ord c :: Int
    if n > (fromIntegral (maxBound :: Word8) :: Int)
        then fail ("Character " ++ [c] ++ " is outside the range of Word8.")
        else lift (Delimiter (fromIntegral n :: Word8))
