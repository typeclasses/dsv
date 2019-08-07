{-# LANGUAGE NoImplicitPrelude #-}

module DSV.ZipViewError
  ( ZipViewError (..)
  ) where

data ZipViewError headerError =

    ZipViewError_Empty
      -- ^ The input contained no rows, not even a header.

  | ZipViewError_HeaderError headerError
      -- ^ There is some problem with the header that would prevent us from interpreting the subsequent rows.
