{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DSV.Tests.FileStrictCsvRead where

import DSV.TestPrelude
import DSV.TestData.Tweets

group :: Group
group = $$(discover)

prop_readCsvFileStrictWithoutHeader_tweets = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets.csv"
        readCsvFileStrictWithoutHeader fp

    result ===
        ( ParseComplete
        , listToVector $ map (listToVector . map encodeTextUtf8)
              [tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5]
        )

-- Corresponds to the example in the documentation for 'readCsvFileStrictWithoutHeader'.
prop_readCsvFileStrictWithoutHeader_doc = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-without-header.csv"
        readCsvFileStrictWithoutHeader fp

    result ===
        ( ParseComplete
        , listToVector $ map (listToVector . map encodeTextUtf8)
            [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
            , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
            ]
        )

-- Corresponds to the second example in the documentation for 'readCsvFileStrictWithoutHeader'.
prop_readCsvFileStrictWithoutHeader_doc_error = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-malformed-without-header.csv"
        readCsvFileStrictWithoutHeader fp

    result ===
        ( ParseIncomplete
        , listToVector $ map (listToVector . map encodeTextUtf8)
            [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
            ]
        )

prop_readCsvFileStrictWithZippedHeader_tweets = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets.csv"
        readCsvFileStrictWithZippedHeader fp

    result ===
        ( ParseComplete
        , listToVector $ map (listToVector . map (bimap encodeTextUtf8 encodeTextUtf8))
              [tweet1_labeled, tweet2_labeled, tweet3_labeled, tweet4_labeled, tweet5_labeled]
        )

-- Corresponds to the example in the documentation for 'readCsvFileStrictWithZippedHeader'.
prop_readCsvFileStrictWithZippedHeader_doc = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        readCsvFileStrictWithZippedHeader fp

    result ===
        ( ParseComplete
        , listToVector $ map (listToVector . map (bimap encodeTextUtf8 encodeTextUtf8))
            [ [ ("Date", "2019-03-24")
              , ("Vendor", "Acme Co")
              , ("Price", "$599.89")
              , ("Product", "Dehydrated boulders")
              ]
            , [ ("Date", "2019-04-18")
              , ("Vendor", "Acme Co")
              , ("Price", "$24.95")
              , ("Product", "Earthquake pills")
              ]
            ]
        )

-- Corresponds to the second example in the documentation for 'readCsvFileStrictWithZippedHeader'.
prop_readCsvFileStrictWithZippedHeader_doc_error = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-malformed-with-header.csv"
        readCsvFileStrictWithZippedHeader fp

    result ===
        ( ParseIncomplete
        , listToVector $ map (listToVector . map (bimap encodeTextUtf8 encodeTextUtf8))
            [ [ ("Date", "2019-03-24")
              , ("Vendor", "Acme Co")
              , ("Price", "$599.89")
              , ("Product", "Dehydrated boulders")
              ]
            ]

        )

prop_readCsvFileStrictIgnoringHeader_tweets = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets.csv"
        readCsvFileStrictIgnoringHeader fp

    result ===
        ( ParseComplete
        , listToVector $ map (listToVector . map encodeTextUtf8)
              [tweet1, tweet2, tweet3, tweet4, tweet5]
        )

-- Corresponds to the example in the documentation for 'readCsvFileStrictIgnoringHeader'.
prop_readCsvFileStrictIgnoringHeader_doc = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        readCsvFileStrictIgnoringHeader fp

    result ===
        ( ParseComplete
        , listToVector $ map (listToVector . map encodeTextUtf8)
            [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
            , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
            ]
        )
