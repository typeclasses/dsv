{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Pipes.Dsv
import Paths_pipes_dsv

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Bifunctor

import Control.Monad (when)
import Control.Monad.IO.Class

import System.IO (hSetEncoding, stdout, stderr, utf8)
import System.Exit (exitFailure)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Vector as Vector

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
    (getDataFileName "test/tweets.csv" >>= readCsvFileStrictWithoutHeader)
    ~>
    ( AttoComplete
    , map (Vector.fromList . map encodeUtf8)
          [tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5]
    )

prop_readCsvFileStrictWithoutHeader_doc =
    (getDataFileName "test/doc-example-without-header.csv" >>= readCsvFileStrictWithoutHeader)
    ~>
    ( AttoComplete
    , map (Vector.fromList . map encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
        ]
    )

prop_readCsvFileStrictUsingHeader_tweets =
    (getDataFileName "test/tweets.csv" >>= readCsvFileStrictUsingHeader)
    ~>
    ( AttoComplete
    , map (Vector.fromList . map (bimap encodeUtf8 encodeUtf8))
          [tweet1_labeled, tweet2_labeled, tweet3_labeled, tweet4_labeled, tweet5_labeled]
    )

prop_readCsvFileStrictUsingHeader_doc =
    (getDataFileName "test/doc-example-with-header.csv" >>= readCsvFileStrictUsingHeader)
    ~>
    ( AttoComplete
    , map (Vector.fromList . map (bimap encodeUtf8 encodeUtf8))
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
    (getDataFileName "test/tweets.csv" >>= readCsvFileStrictIgnoringHeader)
    ~>
    ( AttoComplete
    , map (Vector.fromList . map encodeUtf8)
          [tweet1, tweet2, tweet3, tweet4, tweet5]
    )

prop_readCsvFileStrictIgnoringHeader_doc =
    (getDataFileName "test/doc-example-with-header.csv" >>= readCsvFileStrictIgnoringHeader)
    ~>
    ( AttoComplete
    , map (Vector.fromList . map encodeUtf8)
        [ ["2019-03-24", "Acme Co", "$599.89", "Dehydrated boulders"]
        , ["2019-04-18", "Acme Co", "$24.95", "Earthquake pills"]
        ]
    )

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
