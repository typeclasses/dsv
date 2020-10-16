{-# LANGUAGE NoImplicitPrelude #-}

module DSV.LookupUtf8
  ( lookupTextViewUtf8, lookupStringViewUtf8
  , lookupTextViewUtf8_, lookupStringViewUtf8_
  , LookupErrorUtf8 (..)
  ) where

import DSV.ByteString
import DSV.LookupErrorUtf8
import DSV.Prelude
import DSV.Text
import DSV.Validation
import DSV.Vector
import DSV.ViewType
import DSV.UTF8

lookupTextViewUtf8 :: (Text -> Bool)
    -> View LookupErrorUtf8 (Vector (ByteString, ByteString)) Text
lookupTextViewUtf8 = lookupViewUtf8

lookupTextViewUtf8_ :: (Text -> Bool)
    -> View () (Vector (ByteString, ByteString)) Text
lookupTextViewUtf8_ x = discardViewError (lookupTextViewUtf8 x)

lookupStringViewUtf8 :: (String -> Bool)
    -> View LookupErrorUtf8 (Vector (ByteString, ByteString)) String
lookupStringViewUtf8 = lookupViewUtf8

lookupStringViewUtf8_ :: (String -> Bool)
    -> View () (Vector (ByteString, ByteString)) String
lookupStringViewUtf8_ x = discardViewError (lookupStringViewUtf8 x)

lookupViewUtf8 :: DecodeUtf8 a => (a -> Bool)
    -> View LookupErrorUtf8 (Vector (ByteString, ByteString)) a
lookupViewUtf8 f =
    View $ \xs ->
        case filter (\(n, _) -> maybe False f (decodeUtf8Maybe n)) (toList xs) of
            [] -> Failure LookupErrorUtf8_Missing
            [(_, v)] -> maybe (Failure LookupErrorUtf8_Invalid) Success (decodeUtf8Maybe v)
            _ -> Failure LookupErrorUtf8_Duplicate
