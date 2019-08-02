{-# LANGUAGE NoImplicitPrelude #-}

module Dsv.AttoParser
  ( AttoParser
  ) where

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

type AttoParser = Atto.Parser
