import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.6.5" -> callProcess "cabal" ["test", "dsv"
                  , "--constraint=doctest == 0.16.*"
                  , "--constraint=template-haskell == 2.14.*"
                  ]
      "8.8.4" -> callProcess "cabal" ["test", "dsv"]
      "8.10.2" -> callProcess "cabal" ["test", "dsv"
                  , "--constraint=doctest == 0.17.*"
                  , "--constraint=template-haskell == 2.16.*"
                  ]
