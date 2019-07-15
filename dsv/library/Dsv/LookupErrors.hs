{-# LANGUAGE MultiParamTypeClasses #-}

module Dsv.LookupErrors where

class RowTooShort err
  where
    rowTooShort :: err

class DuplicateColumn str err
  where
    duplicateColumn :: str -> err

class MissingColumn str err
  where
    missingColumn :: str -> err
