{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module DSV.Tests.FileStrictCsvZipView where

import DSV.TestPrelude

group :: Group
group = $$(discover)

-- | This is an example in the documentation for 'zipViewCsvFileStrict'.
prop_zipViewCsvFileStrict_entireRow = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        zipViewCsvFileStrict fp (entireRowZipView @() @())

    result ===
        ( ZipViewComplete
        , listToVector $ map (fmap (listToVector . map encodeTextUtf8)) $
            [ Success [ "2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders" ]
            , Success [ "2019-04-18", "Acme Co", "$24.95", "Earthquake pills" ]
            ]
        )

-- | This is an example in the documentation for 'zipViewCsvFileStrict'.
prop_zipViewCsvFileStrict_particularColumns = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        zipViewCsvFileStrict fp $
          do
            date    <- overZipViewError (:[]) (:[]) (textZipViewUtf8 "Date"    utf8TextView)
            product <- overZipViewError (:[]) (:[]) (textZipViewUtf8 "Product" utf8TextView)
            return (date, product)

    result ===
        ( ZipViewComplete
        , listToVector
            [ Success ("2019-03-24", "Dehydrated boulders")
            , Success ("2019-04-18", "Earthquake pills")
            ]
        )

-- | This is an example in the documentation for 'zipViewCsvFileStrict'.
prop_zipViewCsvFileStrict_particularColumns_utf8Error = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-utf8-errors.csv"
        zipViewCsvFileStrict fp $
          do
            date    <- overZipViewError (:[]) (:[]) (textZipViewUtf8 "Date"    utf8TextView)
            product <- overZipViewError (:[]) (:[]) (textZipViewUtf8 "Product" utf8TextView)
            return (date, product)

    result ===
        ( ZipViewComplete
        , listToVector
            [ Failure [At (ColumnName "Product") (IndexError_FieldError InvalidUtf8)]
            , Success ("2019-04-18", "Earthquake pills")
            ]
        )

prop_zipViewCsvFileStrict_particularColumns_utf8Error_throw = example $
  do
    result <- liftIO $ try $
      do
        fp <- getDataFileName "test-data/doc-example-with-utf8-errors.csv"
        zipViewCsvFileStrictThrowFirstError fp $
          overZipViewError getFirst getFirst $
            do
              date    <- overZipViewError First First (textZipViewUtf8 "Date"    utf8TextView)
              product <- overZipViewError First First (textZipViewUtf8 "Product" utf8TextView)
              return (date, product)

    result ===
        Left
          (At (RowNumber 1)
            (At (ColumnName @Text "Product")
              (IndexError_FieldError InvalidUtf8)))

prop_zipViewCsvFileStrict_empty = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/empty.csv"
        zipViewCsvFileStrict fp (entireRowZipView @() @())

    result === (ZipViewEmpty, emptyVector)

prop_tweetIds = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets.csv"
        zipViewCsvFileStrictThrowFirstError fp $
            textZipViewUtf8 "tweet_id" byteStringNatView

    result ===
        listToVector
          [ 1145722305135423488
          , 1144834283204415488
          , 1144470003963420672
          , 1143630783963389954
          , 1143602240722210823
          ]

prop_tweetIdAndTimestamp = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets.csv"
        zipViewCsvFileStrict fp $
          do
            tweetId <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "tweet_id" byteStringNatView)
            tweetTime <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "timestamp" utf8TextView)
            return (tweetId, tweetTime)

    result ===
        ( ZipViewComplete
        , listToVector $
            [ Success (1145722305135423488, "2019-07-01 15:54:18 +0000")
            , Success (1144834283204415488, "2019-06-29 05:05:37 +0000")
            , Success (1144470003963420672, "2019-06-28 04:58:06 +0000")
            , Success (1143630783963389954, "2019-06-25 21:23:21 +0000")
            , Success (1143602240722210823, "2019-06-25 19:29:55 +0000")
            ]
        )

prop_tweetIdAndTimestamp_rowError = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets-with-id-error.csv"
        zipViewCsvFileStrict fp $
          do
            tweetId <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "tweet_id" byteStringNatView)
            tweetTime <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "timestamp" utf8TextView)
            return (tweetId, tweetTime)

    result ===
        ( ZipViewComplete
        , listToVector $
            [ Success (1145722305135423488, "2019-07-01 15:54:18 +0000")
            , Success (1144834283204415488, "2019-06-29 05:05:37 +0000")
            , Failure ()
            , Success (1143630783963389954, "2019-06-25 21:23:21 +0000")
            , Success (1143602240722210823, "2019-06-25 19:29:55 +0000")
            ]
        )

prop_tweetIdAndTimestamp_missingHeader = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets.csv"
        zipViewCsvFileStrict fp $
          do
            tweetId <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "tweet_id" byteStringNatView)
            tweetTime <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "color" utf8TextView)
            return (tweetId, tweetTime)

    result ===
        ( ZipViewHeaderError [ At (ColumnName "color") LookupError_Missing ]
        , emptyVector
        )

prop_tweetIdAndTimestamp_duplicateHeader = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/tweets-with-duplicate-header.csv"
        zipViewCsvFileStrict fp $
          do
            tweetId <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "tweet_id" byteStringNatView)
            tweetTime <- overZipViewError (:[]) (const ()) (textZipViewUtf8 "timestamp" utf8TextView)
            return (tweetId, tweetTime)

    result ===
        ( ZipViewHeaderError [ At (ColumnName "timestamp") LookupError_Duplicate ]
        , emptyVector
        )
