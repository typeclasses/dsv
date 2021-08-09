import Data.Maybe
import System.Environment
import System.Process

main =
  do
    ghc <- readGHC <$> getEnv "ghc"
    callProcess "cabal" ("build" : "all" : constraints ghc)
    callProcess "cabal" ("test" : "all" : "--enable-tests" : constraints ghc)

x .= Just y  = Just ("--constraint=" ++ x ++ "==" ++ y)
x .= Nothing = Nothing

data GHC = GHC_8_6 | GHC_8_8 | GHC_8_10 | GHC_9_0

readGHC ghcString = case ghcString of
    "8.6"  -> GHC_8_6
    "8.8"  -> GHC_8_8
    "8.10" -> GHC_8_10
    "9.0"  -> GHC_9_0

constraints ghc = catMaybes
    [ "base" .= case ghc of
        GHC_8_6  -> Just "4.12.*"
        GHC_8_8  -> Just "4.13.*"
        GHC_8_10 -> Just "4.14.*"
        GHC_9_0  -> Just "4.15.*"
    , "bytestring" .= case ghc of
        GHC_8_6  -> Just "0.10.*"
        GHC_8_10 -> Just "0.11.*"
        GHC_9_0  -> Just "0.11.*"
        _        -> Nothing
    , "template-haskell" .= case ghc of
        GHC_8_6  -> Just "2.14.*"
        GHC_8_10 -> Just "2.16.*"
        GHC_9_0  -> Just "2.17.*"
        _        -> Nothing
    ]
