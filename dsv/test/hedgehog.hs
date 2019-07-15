{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}

-- Some tests are roughly equivalent to examples given in the library documentation. If this is the case, it is noted in a comment next to the test. Please be sure to keep these tests consistent with the documentation when either changes.

import Dsv
import Paths_dsv

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad ((>=>))

import Data.IORef
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Foldable (toList, traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.IO.Class

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
  checkParallel $$(discover)

example =
    withTests 1 . property

x ~> y =
    example (liftIO x >>= (=== y))

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- tests
    when (not ok) exitFailure

prop_readCsvFileStrictWithoutHeader_tweets =
    (
      do
        fp <- getDataFileName "test-data/tweets.csv"
        readCsvFileStrictWithoutHeader fp
    )
    ~>
    ( ParseComplete
    , Vector.fromList $ map (Vector.fromList . map Text.encodeUtf8)
          [tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5]
    )

-- Corresponds to the example in the documentation for 'readCsvFileStrictWithoutHeader'.
prop_readCsvFileStrictWithoutHeader_doc =
    (
      do
        fp <- getDataFileName "test-data/doc-example-without-header.csv"
        readCsvFileStrictWithoutHeader fp
        ------------------------------
    )
    ~>
    ( ParseComplete
    , Vector.fromList $ map (Vector.fromList . map Text.encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
        ]
    )

-- Corresponds to the second example in the documentation for 'readCsvFileStrictWithoutHeader'.
prop_readCsvFileStrictWithoutHeader_doc_error =
    (
      do
        fp <- getDataFileName "test-data/doc-example-malformed-without-header.csv"
        readCsvFileStrictWithoutHeader fp
        ------------------------------
    )
    ~>
    ( ParseIncomplete
    , Vector.fromList $ map (Vector.fromList . map Text.encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        ]
    )

prop_readCsvFileStrictWithZippedHeader_tweets =
    (
      do
        fp <- getDataFileName "test-data/tweets.csv"
        readCsvFileStrictWithZippedHeader fp
    )
    ~>
    ( ParseComplete
    , Vector.fromList $ map (Vector.fromList . map (bimap Text.encodeUtf8 Text.encodeUtf8))
          [tweet1_labeled, tweet2_labeled, tweet3_labeled, tweet4_labeled, tweet5_labeled]
    )

-- Corresponds to the example in the documentation for 'readCsvFileStrictWithZippedHeader'.
prop_readCsvFileStrictWithZippedHeader_doc =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        readCsvFileStrictWithZippedHeader fp
        ---------------------------------
    )
    ~>
    ( ParseComplete
    , Vector.fromList $ map (Vector.fromList . map (bimap Text.encodeUtf8 Text.encodeUtf8))
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
prop_readCsvFileStrictWithZippedHeader_doc_error =
    (
      do
        fp <- getDataFileName "test-data/doc-example-malformed-with-header.csv"
        readCsvFileStrictWithZippedHeader fp
        ---------------------------------
    )
    ~>
    ( ParseIncomplete
    , Vector.fromList $ map (Vector.fromList . map (bimap Text.encodeUtf8 Text.encodeUtf8))
        [ [ ("Date", "2019-03-24")
          , ("Vendor", "Acme Co")
          , ("Price", "$599.89")
          , ("Product", "Dehydrated boulders")
          ]
        ]

    )

prop_readCsvFileStrictIgnoringHeader_tweets =
    (
      do
        fp <- getDataFileName "test-data/tweets.csv"
        readCsvFileStrictIgnoringHeader fp
    )
    ~>
    ( ParseComplete
    , Vector.fromList $ map (Vector.fromList . map Text.encodeUtf8)
          [tweet1, tweet2, tweet3, tweet4, tweet5]
    )

-- Corresponds to the example in the documentation for 'readCsvFileStrictIgnoringHeader'.
prop_readCsvFileStrictIgnoringHeader_doc =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        readCsvFileStrictIgnoringHeader fp
        -------------------------------
    )
    ~>
    ( ParseComplete
    , Vector.fromList $ map (Vector.fromList . map Text.encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
        ]
    )

sumPricesWithoutHeader :: L.Fold (Vector ByteString) Rational
sumPricesWithoutHeader =
    L.premap
        (fromMaybe 0 . (nthColumn 3 >=> byteStringDollarsMaybe))
        L.sum

sumPricesWithZippedHeader :: L.Fold (Vector (ByteString, ByteString)) Rational
sumPricesWithZippedHeader =
    L.premap
        (fromMaybe 0 . (columnName "Price" >=> byteStringDollarsMaybe))
        L.sum

writeNamesAndCountWithoutHeader :: IORef (Seq ByteString) -> L.FoldM IO (Vector ByteString) Int
writeNamesAndCountWithoutHeader r =
    L.mapM_ (traverse_ write . nthColumn 4) *>
    L.generalize L.length
  where
    write x = modifyIORef r (<> Seq.singleton x)

writeNamesAndCountWithZippedHeader :: IORef (Seq ByteString) -> L.FoldM IO (Vector (ByteString, ByteString)) Int
writeNamesAndCountWithZippedHeader r =
    L.mapM_ (traverse_ write . columnName "Product") *>
    L.generalize L.length
  where
    write x = modifyIORef r (<> Seq.singleton x)

-- Corresponds to the example in the documentation for 'foldCsvFileWithoutHeader'.
prop_foldPrice_withoutHeader_doc =
    (
      do
        fp <- getDataFileName "test-data/doc-example-without-header.csv"
        foldCsvFileWithoutHeader fp sumPricesWithoutHeader
        ------------------------
    )
    ~>
    (ParseComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileWithoutHeaderM'.
prop_foldPriceM_withoutHeader_doc =
    (
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test-data/doc-example-without-header.csv"
        (t, n) <- foldCsvFileWithoutHeaderM fp (writeNamesAndCountWithoutHeader r)
                  -------------------------
        rs <- readIORef r
        return (t, toList rs, n)
    )
    ~>
    (ParseComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- Corresponds to the example in the documentation for 'foldCsvFileIgnoringHeader'.
prop_foldPrice_ignoringHeader_doc =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        foldCsvFileIgnoringHeader fp sumPricesWithoutHeader
        -------------------------
    )
    ~>
    (ParseComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileIgnoringHeaderM'.
prop_foldPriceM_ignoringHeader_doc =
    (
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        (t, n) <- foldCsvFileIgnoringHeaderM fp (writeNamesAndCountWithoutHeader r)
                  --------------------------
        rs <- readIORef r
        return (t, toList rs, n)
    )
    ~>
    (ParseComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- Corresponds to the example in the documentation for 'foldCsvFileWithZippedHeader'.
prop_foldPrice_withZippedHeader_doc =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        foldCsvFileWithZippedHeader fp sumPricesWithZippedHeader
        ---------------------------
    )
    ~>
    (ParseComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileWithZippedHeaderM'.
prop_foldPriceM_withZippedHeader_doc =
    (
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        (t, n) <- foldCsvFileWithZippedHeaderM fp (writeNamesAndCountWithZippedHeader r)
                  ----------------------------
        rs <- readIORef r
        return (t, toList rs, n)
    )
    ~>
    (ParseComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- | This is an example in the documentation for 'lookupCsvFileStrict'.
prop_lookupCsvFileStrict_entireRow =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        lookupCsvFileStrict fp (entireRow :: Lookup () () (Vector ByteString))
        -------------------
    )
    ~>
    ( ParseLookupComplete
    , Vector.fromList $ map (fmap (Vector.fromList . map Text.encodeUtf8)) $
        [ Success [ "2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders" ]
        , Success [ "2019-04-18", "Acme Co", "$24.95", "Earthquake pills" ]
        ]
    )

-- | This is an example in the documentation for 'lookupCsvFileStrict'.
prop_lookupCsvFileStrict_particularColumns =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        lookupCsvFileStrict fp
          (
            ((,) <$> columnUtf8 "Date" <*> columnUtf8 "Product")
            :: Lookup EnglishText EnglishText (Text, Text)
          )
        -------------------
    )
    ~>
    ( ParseLookupComplete
    , Vector.fromList
        [ Success ("2019-03-24", "Dehydrated boulders")
        , Success ("2019-04-18", "Earthquake pills")
        ]
    )

-- | This is an example in the documentation for 'lookupCsvFileStrict'.
prop_lookupCsvFileStrict_particularColumns_utf8Error =
    (
      do
        fp <- getDataFileName "test-data/doc-example-with-utf8-errors.csv"
        lookupCsvFileStrict fp
          (
            ((,) <$> columnUtf8 "Date" <*> columnUtf8 "Product")
            :: Lookup EnglishText EnglishText (Text, Text)
          )
        -------------------
    )
    ~>
    ( ParseLookupComplete
    , Vector.fromList
        [ Failure (EnglishText ["The byte string in column named 'Product' is not valid UTF-8."])
        , Success ("2019-04-18", "Earthquake pills")
        ]
    )

prop_lookupCsvFileStrict_empty =
    (
      do
        fp <- getDataFileName "test-data/empty.csv"
        lookupCsvFileStrict fp (entireRow :: Lookup () () (Vector ByteString))
    )
    ~>
    (ParseLookupEmpty, Vector.empty)

tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5 :: [Text]

tweet1_labeled, tweet2_labeled, tweet3_labeled, tweet4_labeled, tweet5_labeled :: [(Text, Text)]

labeled = (,)

tweetsHeader =
    [ "tweet_id"
    , "in_reply_to_status_id"
    , "in_reply_to_user_id"
    , "timestamp"
    , "source"
    , "text"
    , "retweeted_status_id"
    , "retweeted_status_user_id"
    , "retweeted_status_timestamp"
    , "expanded_urls"
    ]

tweet1 =
    [ "1145722305135423488"
    , ""
    , ""
    , "2019-07-01 15:54:18 +0000"
    , "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , "stack overflow should just permaban you if you say the word \"efficient\" to somebody whose code still doesn't give correct results"
    , ""
    , ""
    , ""
    , ""
    ]

tweet1_labeled =
    [ labeled "tweet_id" "1145722305135423488"
    , labeled "in_reply_to_status_id" ""
    , labeled "in_reply_to_user_id" ""
    , labeled "timestamp" "2019-07-01 15:54:18 +0000"
    , labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , labeled "text" "stack overflow should just permaban you if you say the word \"efficient\" to somebody whose code still doesn't give correct results"
    , labeled "retweeted_status_id" ""
    , labeled "retweeted_status_user_id" ""
    , labeled "retweeted_status_timestamp" ""
    , labeled "expanded_urls" ""
    ]

tweet2 =
    [ "1144834283204415488"
    , ""
    , ""
    , "2019-06-29 05:05:37 +0000"
    , "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , "INI files are extremely underrated IMO"
    , ""
    , ""
    , ""
    , ""
    ]

tweet2_labeled =
    [ labeled "tweet_id" "1144834283204415488"
    , labeled "in_reply_to_status_id" ""
    , labeled "in_reply_to_user_id" ""
    , labeled "timestamp" "2019-06-29 05:05:37 +0000"
    , labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , labeled "text" "INI files are extremely underrated IMO"
    , labeled "retweeted_status_id" ""
    , labeled "retweeted_status_user_id" ""
    , labeled "retweeted_status_timestamp" ""
    , labeled "expanded_urls" ""
    ]

tweet3 =
    [ "1144470003963420672"
    , ""
    , ""
    , "2019-06-28 04:58:06 +0000"
    , "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , "I'll listen to any song that has a trumpet solo in the middle"
    , ""
    , ""
    , ""
    , ""
    ]

tweet3_labeled =
    [ labeled "tweet_id" "1144470003963420672"
    , labeled "in_reply_to_status_id" ""
    , labeled "in_reply_to_user_id" ""
    , labeled "timestamp" "2019-06-28 04:58:06 +0000"
    , labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , labeled "text" "I'll listen to any song that has a trumpet solo in the middle"
    , labeled "retweeted_status_id" ""
    , labeled "retweeted_status_user_id" ""
    , labeled "retweeted_status_timestamp" ""
    , labeled "expanded_urls" ""
    ]

tweet4 =
    [ "1143630783963389954"
    , ""
    , ""
    , "2019-06-25 21:23:21 +0000"
    , "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , "just accidentally typed \"the Python Report\" instead of \"the Python documentation\""
    , ""
    , ""
    , ""
    , ""
    ]

tweet4_labeled =
    [ labeled "tweet_id" "1143630783963389954"
    , labeled "in_reply_to_status_id" ""
    , labeled "in_reply_to_user_id" ""
    , labeled "timestamp" "2019-06-25 21:23:21 +0000"
    , labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , labeled "text" "just accidentally typed \"the Python Report\" instead of \"the Python documentation\""
    , labeled "retweeted_status_id" ""
    , labeled "retweeted_status_user_id" ""
    , labeled "retweeted_status_timestamp" ""
    , labeled "expanded_urls" ""
    ]

tweet5 =
    [ "1143602240722210823"
    , ""
    , ""
    , "2019-06-25 19:29:55 +0000"
    , "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , "I doubt a take about why a programming language is the best one has ever elicited action... point out a single cool feature or use that can catch someone's interest"
    , ""
    , ""
    , ""
    , ""
    ]

tweet5_labeled =
    [ labeled "tweet_id" "1143602240722210823"
    , labeled "in_reply_to_status_id" ""
    , labeled "in_reply_to_user_id" ""
    , labeled "timestamp" "2019-06-25 19:29:55 +0000"
    , labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , labeled "text" "I doubt a take about why a programming language is the best one has ever elicited action... point out a single cool feature or use that can catch someone's interest"
    , labeled "retweeted_status_id" ""
    , labeled "retweeted_status_user_id" ""
    , labeled "retweeted_status_timestamp" ""
    , labeled "expanded_urls" ""
    ]
