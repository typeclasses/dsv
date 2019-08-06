module DSV.AttoView
  ( attoByteStringView
  ) where

import DSV.AttoParser
import DSV.ByteString
import DSV.Validation
import DSV.ViewType

-- attoparsec
import qualified Data.Attoparsec.ByteString as A

attoByteStringView :: e -> AttoParser a -> View e ByteString a
attoByteStringView e p =
  View $ \bs ->
    case A.parseOnly (p <* A.endOfInput) bs of
      Left _ -> Failure e
      Right x -> Success x
