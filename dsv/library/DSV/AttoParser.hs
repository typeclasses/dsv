{-# LANGUAGE NoImplicitPrelude #-}

module DSV.AttoParser
  ( AttoParser
  ) where

-- attoparsec
import qualified Data.Attoparsec.ByteString as Atto

type AttoParser = Atto.Parser
