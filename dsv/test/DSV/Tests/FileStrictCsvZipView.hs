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
        , listToVector $ map (fmap (listToVector . map (encodeUtf8 @Text))) $
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
            date    <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Date"    (utf8View @Text))
            product <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Product" (utf8View @Text))
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
            date    <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Date"    (utf8View @Text))
            product <- overZipViewError (:[]) (:[]) (textZipViewUtf8 @Text "Product" (utf8View @Text))
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
              date    <- overZipViewError First First (textZipViewUtf8 @Text "Date"    (utf8View @Text))
              product <- overZipViewError First First (textZipViewUtf8 @Text "Product" (utf8View @Text))
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
        zipViewCsvFileStrictThrowFirstError fp
            (textZipViewUtf8 @Text "tweet_id" byteStringNatView)

    result ===
        listToVector
          [ 1145722305135423488
          , 1144834283204415488
          , 1144470003963420672
          , 1143630783963389954
          , 1143602240722210823
          ]
