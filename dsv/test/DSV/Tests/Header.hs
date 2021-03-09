{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module DSV.Tests.Header where

import DSV.TestPrelude

import qualified Pipes.Prelude as P

group :: Group
group = $$(discover)

-- Corresponds to the example in the documentation for 'zipHeader'.
prop_zipHeaderPipe_doc = example $
  do
    let r1 = listToVector ["A","B"] :: Vector String
    let r2 = listToVector ["1","2"]
    let r3 = listToVector ["3","4"]
    let p = do { yield r1; yield r2; yield r3 }
    result <- P.toListM (p >-> zipHeaderPipe)
    result ===
        [ [("A","1"),("B","2")]
        , [("A","3"),("B","4")]
        ]

-- Corresponds to the example in the documentation for 'zipHeaderWithPipe'.
prop_zipHeaderWithPipe_doc = example $
  do
    let r1 = listToVector ["A","B"] :: Vector String
    let r2 = listToVector ["1","2"]
    let r3 = listToVector ["3","4"]
    let p = do { yield r1; yield r2; yield r3 }
    result <- P.toListM (p >-> zipHeaderWithPipe (<>))
    result === [ ["A1","B2"], ["A3","B4"] ]
