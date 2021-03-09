import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.6.5" -> callProcess "cabal" ["test", "dsv"
                  , "--constraint=bytestring == 0.10.*"
                  , "--constraint=template-haskell == 2.14.*"
                  ]
      "8.8.4" -> callProcess "cabal" ["test", "dsv"]
      "8.10.2" -> callProcess "cabal" ["test", "dsv"
                  , "--constraint=bytestring == 0.11.*"
                  , "--constraint=template-haskell == 2.16.*"
                  ]
