import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-ilibrary"
    , "library/DSV/Header.hs"
    ]