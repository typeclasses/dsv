{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

module Dsv.LookupErrors where

class DsvError a b where
  dsvError :: a -> b

data RowTooShort = RowTooShort

data DuplicateColumn str = DuplicateColumn str -- ^ Column name

data MissingColumn str = MissingColumn str -- ^ Column name

data InvalidUtf8 str = InvalidUtf8 str -- ^ Column name
