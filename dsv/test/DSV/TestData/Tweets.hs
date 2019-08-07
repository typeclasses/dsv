{-# LANGUAGE OverloadedStrings #-}

module DSV.TestData.Tweets where

import Data.Text (Text)

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
