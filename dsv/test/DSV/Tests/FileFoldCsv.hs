{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DSV.Tests.FileFoldCsv where

import DSV.TestPrelude

import qualified Control.Foldl as L

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

group :: Group
group = $$(discover)

sumPricesWithoutHeader :: L.Fold (Vector ByteString) Rational
sumPricesWithoutHeader =
    L.premap
        (viewOr 0 $
            discardViewError byteStringDollarsView <<<
            discardViewError (columnNumberView 3)
        )
        L.sum

sumPricesWithZippedHeader :: L.Fold (Vector (ByteString, ByteString)) Rational
sumPricesWithZippedHeader =
    L.premap
        (viewOr 0 $
            discardViewError byteStringDollarsView <<<
            discardViewError (lookupView (== "Price"))
        )
        L.sum

writeNamesAndCountWithoutHeader :: IORef (Seq ByteString) -> L.FoldM IO (Vector ByteString) Int
writeNamesAndCountWithoutHeader r =
    L.mapM_ (traverse_ write . nthVectorElement 4) *>
    L.generalize L.length
  where
    write x = modifyIORef r (<> Seq.singleton x)

writeNamesAndCountWithZippedHeader :: IORef (Seq ByteString) -> L.FoldM IO (Vector (ByteString, ByteString)) Int
writeNamesAndCountWithZippedHeader r =
    L.mapM_ (traverse_ write . vectorLookup (== "Product")) *>
    L.generalize L.length
  where
    write x = modifyIORef r (<> Seq.singleton x)

-- Corresponds to the example in the documentation for 'foldCsvFileWithoutHeader'.
prop_foldPrice_withoutHeader_doc = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-without-header.csv"
        foldCsvFileWithoutHeader fp sumPricesWithoutHeader

    result === (ParseComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileWithoutHeaderM'.
prop_foldPriceM_withoutHeader_doc = example $
  do
    result <- liftIO $
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test-data/doc-example-without-header.csv"
        (t, n) <- foldCsvFileWithoutHeaderM fp (writeNamesAndCountWithoutHeader r)
        rs <- readIORef r
        return (t, toList rs, n)

    result === (ParseComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- Corresponds to the example in the documentation for 'foldCsvFileIgnoringHeader'.
prop_foldPrice_ignoringHeader_doc = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        foldCsvFileIgnoringHeader fp sumPricesWithoutHeader

    result === (ParseComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileIgnoringHeaderM'.
prop_foldPriceM_ignoringHeader_doc = example $
  do
    result <- liftIO $
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        (t, n) <- foldCsvFileIgnoringHeaderM fp (writeNamesAndCountWithoutHeader r)
        rs <- readIORef r
        return (t, toList rs, n)

    result === (ParseComplete, ["Dehydrated boulders", "Earthquake pills"], 2)

-- Corresponds to the example in the documentation for 'foldCsvFileWithZippedHeader'.
prop_foldPrice_withZippedHeader_doc = example $
  do
    result <- liftIO $
      do
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        foldCsvFileWithZippedHeader fp sumPricesWithZippedHeader

    result === (ParseComplete, 624.84)

-- Corresponds to the example in the documentation for 'foldCsvFileWithZippedHeaderM'.
prop_foldPriceM_withZippedHeader_doc = example $
  do
    result <- liftIO $
      do
        r <- newIORef Seq.empty
        fp <- getDataFileName "test-data/doc-example-with-header.csv"
        (t, n) <- foldCsvFileWithZippedHeaderM fp (writeNamesAndCountWithZippedHeader r)
        rs <- readIORef r
        return (t, toList rs, n)

    result === (ParseComplete, ["Dehydrated boulders", "Earthquake pills"], 2)
