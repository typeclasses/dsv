{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import DSV.TestPrelude

import qualified DSV.Tests.FileFoldCsv
import qualified DSV.Tests.FileStrictCsvRead
import qualified DSV.Tests.FileStrictCsvZipView

import Control.Monad (when)

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

tests :: IO Bool
tests =
  checkParallel
    (Group
      { groupName = "DSV"
      , groupProperties =
          foldMap groupProperties
            [ DSV.Tests.FileFoldCsv.group
            , DSV.Tests.FileStrictCsvRead.group
            , DSV.Tests.FileStrictCsvZipView.group
            ]
      })

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- tests
    when (not ok) exitFailure
