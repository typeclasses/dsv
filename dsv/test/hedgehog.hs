{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}

-- Some tests are roughly equivalent to examples given in the library documentation. If this is the case, it is noted in a comment next to the test. Please be sure to keep these tests consistent with the documentation when either changes.

import Pipes.Dsv
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
        fp <- getDataFileName "test/tweets.csv"
        readCsvFileStrictWithoutHeader fp
    )
    ~>
    ( AttoComplete
    , map (Vector.fromList . map Text.encodeUtf8)
          [tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5]
    )

-- Corresponds to the example in the documentation for 'readCsvFileStrictWithoutHeader'.
prop_readCsvFileStrictWithoutHeader_doc =
    (
      do
        fp <- getDataFileName "test/doc-example-without-header.csv"
        readCsvFileStrictWithoutHeader fp
        ------------------------------
    )
    ~>
    ( AttoComplete
    , map (Vector.fromList . map Text.encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
        ]
    )

prop_readCsvFileStrictUsingHeader_tweets =
    (
      do
        fp <- getDataFileName "test/tweets.csv"
        readCsvFileStrictUsingHeader fp
    )
    ~>
    ( AttoComplete
    , map (Vector.fromList . map (bimap Text.encodeUtf8 Text.encodeUtf8))
          [tweet1_labeled, tweet2_labeled, tweet3_labeled, tweet4_labeled, tweet5_labeled]
    )

-- Corresponds to the example in the documentation for 'readCsvFileStrictUsingHeader'.
prop_readCsvFileStrictUsingHeader_doc =
    (
      do
        fp <- getDataFileName "test/doc-example-with-header.csv"
        readCsvFileStrictUsingHeader fp
        ----------------------------
    )
    ~>
    ( AttoComplete
    , map (Vector.fromList . map (bimap Text.encodeUtf8 Text.encodeUtf8))
        [ [ Labeled "Date" "2019-03-24"
          , Labeled "Vendor" "Acme Co"
          , Labeled "Price" "$599.89"
          , Labeled "Notes" "Dehydrated boulders"
          ]
        , [ Labeled "Date" "2019-04-18"
          , Labeled "Vendor" "Acme Co"
          , Labeled "Price" "$24.95"
          , Labeled "Notes" "Earthquake pills"
          ]
        ]

    )

prop_readCsvFileStrictIgnoringHeader_tweets =
    (
      do
        fp <- getDataFileName "test/tweets.csv"
        readCsvFileStrictIgnoringHeader fp
    )
    ~>
    ( AttoComplete
    , map (Vector.fromList . map Text.encodeUtf8)
          [tweet1, tweet2, tweet3, tweet4, tweet5]
    )

-- Corresponds to the example in the documentation for 'readCsvFileStrictIgnoringHeader'.
prop_readCsvFileStrictIgnoringHeader_doc =
    (
      do
        fp <- getDataFileName "test/doc-example-with-header.csv"
        readCsvFileStrictIgnoringHeader fp
        -------------------------------
    )
    ~>
    ( AttoComplete
    , map (Vector.fromList . map Text.encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
        ]
    )

sumPricesWithoutHeader :: L.Fold (Vector ByteString) Rational
sumPricesWithoutHeader =
    L.premap
        (fromMaybe 0 . (nthColumn 3 >=> byteStringDollarsMaybe))
        L.sum

sumPricesUsingHeader :: L.Fold (Vector (Labeled ByteString ByteString)) Rational
sumPricesUsingHeader =
    L.premap
        (fromMaybe 0 . (columnName "Price" >=> byteStringDollarsMaybe))
        L.sum

writeNamesAndCountWithoutHeader :: IORef (Seq ByteString) -> L.FoldM IO (Vector ByteString) Int
writeNamesAndCountWithoutHeader r =
    L.mapM_ (traverse_ write . nthColumn 4) *>
    L.generalize L.length
  where
    write x = modifyIORef r (<> Seq.singleton x)

writeNamesAndCountUsingHeader :: IORef (Seq ByteString) -> L.FoldM IO (Vector (Labeled ByteString ByteString)) Int
writeNamesAndCountUsingHeader r =
    L.mapM_ (traverse_ write . columnName "Notes") *>
    L.generalize L.length
  where
    write x = modifyIORef r (<> Seq.singleton x)

-- Corresponds to the example in the documentation for 'foldCsvFileWithoutHeader'.
prop_foldPrice_withoutHeader_doc =
    (
      do
        fp <- getDataFileName "test/doc-example-without-header.csv"
        foldCsvFileWithoutHeader fp sumPricesWithoutHeader
        ------------------------
    )
    ~>
    (AttoComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileWithoutHeaderM'.
prop_foldPriceM_withoutHeader_doc =
    (
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test/doc-example-without-header.csv"
        (t, n) <- foldCsvFileWithoutHeaderM fp (writeNamesAndCountWithoutHeader r)
                  -------------------------
        rs <- readIORef r
        return (t, toList rs, n)
    )
    ~>
    (AttoComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- Corresponds to the example in the documentation for 'foldCsvFileIgnoringHeader'.
prop_foldPrice_ignoringHeader_doc =
    (
      do
        fp <- getDataFileName "test/doc-example-with-header.csv"
        foldCsvFileIgnoringHeader fp sumPricesWithoutHeader
        -------------------------
    )
    ~>
    (AttoComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileIgnoringHeaderM'.
prop_foldPriceM_ignoringHeader_doc =
    (
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test/doc-example-with-header.csv"
        (t, n) <- foldCsvFileIgnoringHeaderM fp (writeNamesAndCountWithoutHeader r)
                  --------------------------
        rs <- readIORef r
        return (t, toList rs, n)
    )
    ~>
    (AttoComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- Corresponds to the example in the documentation for 'foldCsvFileUsingHeader'.
prop_foldPrice_usingHeader_doc =
    (
      do
        fp <- getDataFileName "test/doc-example-with-header.csv"
        foldCsvFileUsingHeader fp sumPricesUsingHeader
        ----------------------
    )
    ~>
    (AttoComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileUsingHeaderM'.
prop_foldPriceM_usingHeader_doc =
    (
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test/doc-example-with-header.csv"
        (t, n) <- foldCsvFileUsingHeaderM fp (writeNamesAndCountUsingHeader r)
                  -----------------------
        rs <- readIORef r
        return (t, toList rs, n)
    )
    ~>
    (AttoComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5 :: [Text]

tweet1_labeled, tweet2_labeled, tweet3_labeled, tweet4_labeled, tweet5_labeled :: [Labeled Text Text]

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
    [ Labeled "tweet_id" "1145722305135423488"
    , Labeled "in_reply_to_status_id" ""
    , Labeled "in_reply_to_user_id" ""
    , Labeled "timestamp" "2019-07-01 15:54:18 +0000"
    , Labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , Labeled "text" "stack overflow should just permaban you if you say the word \"efficient\" to somebody whose code still doesn't give correct results"
    , Labeled "retweeted_status_id" ""
    , Labeled "retweeted_status_user_id" ""
    , Labeled "retweeted_status_timestamp" ""
    , Labeled "expanded_urls" ""
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
    [ Labeled "tweet_id" "1144834283204415488"
    , Labeled "in_reply_to_status_id" ""
    , Labeled "in_reply_to_user_id" ""
    , Labeled "timestamp" "2019-06-29 05:05:37 +0000"
    , Labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , Labeled "text" "INI files are extremely underrated IMO"
    , Labeled "retweeted_status_id" ""
    , Labeled "retweeted_status_user_id" ""
    , Labeled "retweeted_status_timestamp" ""
    , Labeled "expanded_urls" ""
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
    [ Labeled "tweet_id" "1144470003963420672"
    , Labeled "in_reply_to_status_id" ""
    , Labeled "in_reply_to_user_id" ""
    , Labeled "timestamp" "2019-06-28 04:58:06 +0000"
    , Labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , Labeled "text" "I'll listen to any song that has a trumpet solo in the middle"
    , Labeled "retweeted_status_id" ""
    , Labeled "retweeted_status_user_id" ""
    , Labeled "retweeted_status_timestamp" ""
    , Labeled "expanded_urls" ""
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
    [ Labeled "tweet_id" "1143630783963389954"
    , Labeled "in_reply_to_status_id" ""
    , Labeled "in_reply_to_user_id" ""
    , Labeled "timestamp" "2019-06-25 21:23:21 +0000"
    , Labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , Labeled "text" "just accidentally typed \"the Python Report\" instead of \"the Python documentation\""
    , Labeled "retweeted_status_id" ""
    , Labeled "retweeted_status_user_id" ""
    , Labeled "retweeted_status_timestamp" ""
    , Labeled "expanded_urls" ""
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
    [ Labeled "tweet_id" "1143602240722210823"
    , Labeled "in_reply_to_status_id" ""
    , Labeled "in_reply_to_user_id" ""
    , Labeled "timestamp" "2019-06-25 19:29:55 +0000"
    , Labeled "source" "<a href=\"https://about.twitter.com/products/tweetdeck\" rel=\"nofollow\">TweetDeck</a>"
    , Labeled "text" "I doubt a take about why a programming language is the best one has ever elicited action... point out a single cool feature or use that can catch someone's interest"
    , Labeled "retweeted_status_id" ""
    , Labeled "retweeted_status_user_id" ""
    , Labeled "retweeted_status_timestamp" ""
    , Labeled "expanded_urls" ""
    ]
