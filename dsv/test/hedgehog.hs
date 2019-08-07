{-# LANGUAGE ApplicativeDo, OverloadedStrings, TemplateHaskell, TypeApplications #-}

import DSV.TestPrelude
import DSV.TestData.Tweets

import qualified DSV.Tests.FileFoldCsv
import qualified DSV.Tests.FileStrictCsvRead

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Prelude hiding (product)

import Control.Exception.Safe (try)

import Data.Monoid (Sum (..))
import Data.Semigroup (First (..))
import Data.Void

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.IO.Class

import Numeric.Natural

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Control.Foldl as L

import Control.Foldl (Fold (Fold))

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

tests :: IO Bool
tests =
  checkParallel
    (Group
      { groupName = "DSV"
      , groupProperties =
          foldMap groupProperties
            [ DSV.Tests.FileFoldCsv.group
            , DSV.Tests.FileStrictCsvRead.group
            , $$(discover)
            ]
      })

x ~> y =
    example (liftIO x >>= (=== y))

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- tests
    when (not ok) exitFailure

-- | This is an example in the documentation for 'zipViewCsvFileStrict'.
prop_zipViewCsvFileStrict_entireRow =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        zipViewCsvFileStrict fp (entireRowZipView @Void @Void)
        --------------------
    )
    ~>
    ( ZipViewComplete
    , Vector.fromList $ map (fmap (Vector.fromList . map Text.encodeUtf8)) $
        [ Success [ "2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders" ]
        , Success [ "2019-04-18", "Acme Co", "$24.95", "Earthquake pills" ]
        ]
    )

-- | This is an example in the documentation for 'zipViewCsvFileStrict'.
prop_zipViewCsvFileStrict_particularColumns =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        zipViewCsvFileStrict fp $
        --------------------
          do
            date    <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Date"    (utf8View @Text))
            product <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Product" (utf8View @Text))
            return (date, product)
    )
    ~>
    ( ZipViewComplete
    , Vector.fromList
        [ Success ("2019-03-24", "Dehydrated boulders")
        , Success ("2019-04-18", "Earthquake pills")
        ]
    )

-- | This is an example in the documentation for 'zipViewCsvFileStrict'.
prop_zipViewCsvFileStrict_particularColumns_utf8Error =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-utf8-errors.csv"
        zipViewCsvFileStrict fp $
        --------------------
          do
            date    <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Date"    (utf8View @Text))
            product <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Product" (utf8View @Text))
            return (date, product)
    )
    ~>
    ( ZipViewComplete
    , Vector.fromList
        [ Failure [At (ColumnName "Product") (IndexError_FieldError InvalidUtf8)]
        , Success ("2019-04-18", "Earthquake pills")
        ]
    )

prop_zipViewCsvFileStrict_particularColumns_utf8Error_throw =
    (
      try $
        do
          fp <- getDataFileName "test-data/doc-example-with-utf8-errors.csv"
          zipViewCsvFileStrictThrowFirstError fp $
            overZipViewError getFirst getFirst $
              do
                date    <- overZipViewError First First (textZipViewUtf8 @Text "Date"    (utf8View @Text))
                product <- overZipViewError First First (textZipViewUtf8 @Text "Product" (utf8View @Text))
                return (date, product)
    )
    ~>
    Left (At (RowNumber 1) (At (ColumnName @Text "Product") (IndexError_FieldError InvalidUtf8)))

prop_zipViewCsvFileStrict_empty =
    (
      do
        fp <- getDataFileName "test-data/empty.csv"
        zipViewCsvFileStrict fp (entireRowZipView @() @())
    )
    ~>
    (ZipViewEmpty, Vector.empty)

prop_tweetIds =
    (
      do
        fp <- getDataFileName "test-data/tweets.csv"
        zipViewCsvFileStrictThrowFirstError fp
            (textZipViewUtf8 @Text "tweet_id" byteStringNatView)
    )
    ~>
    Vector.fromList
      [ 1145722305135423488
      , 1144834283204415488
      , 1144470003963420672
      , 1143630783963389954
      , 1143602240722210823
      ]
