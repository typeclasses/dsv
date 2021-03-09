{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DSV.Tests.NumberViews where

import DSV.TestPrelude

import Data.Ratio

group :: Group
group = $$(discover)

-- Corresponds to the examples in the documentation for 'byteStringRationalView'.
prop_byteStringRationalView_doc = example $
  do
    applyView byteStringRationalView "1234" === Success (1234 % 1)
    applyView byteStringRationalView "1234.567" === Success (1234567 % 1000)
    applyView byteStringRationalView "12.3.4" === Failure InvalidRational

-- Corresponds to the examples in the documentation for 'textRationalView'.
prop_textRationalView_doc = example $
  do
    applyView textRationalView "1234" === Success (1234 % 1)
    applyView textRationalView "1234.567" === Success (1234567 % 1000)
    applyView textRationalView "12.3.4" === Failure InvalidRational

-- Corresponds to the examples in the documentation for 'byteStringDollarsView'.
prop_byteStringDollarsView_doc = example $
  do
    applyView byteStringDollarsView "$1234.567" === Success (1234567 % 1000)
    applyView byteStringDollarsView "1234.567" === Failure InvalidDollars

-- Corresponds to the examples in the documentation for 'textDollarsView'.
prop_textDollarsView_doc = example $
  do
    applyView textDollarsView "$1234.567" === Success (1234567 % 1000)
    applyView textDollarsView "1234.567" === Failure InvalidDollars
