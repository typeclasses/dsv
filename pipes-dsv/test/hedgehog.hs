{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Pipes.Dsv
import Paths_pipes_dsv

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

prop_1 =
    (getDataFileName "test/tweets.csv" >>= readCsvFileStrictWithoutHeader)
    ~>
    (AttoComplete, map (Vector.fromList . map encodeUtf8) [tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5])

tweetsHeader, tweet1, tweet2, tweet3, tweet4, tweet5 :: [Text]

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
