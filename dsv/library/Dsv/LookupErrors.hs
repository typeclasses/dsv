{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

module Dsv.LookupErrors where

class RowTooShort err
  where
    rowTooShort :: err

class DuplicateColumn str err
  where
    duplicateColumn
        :: str  -- ^ Column name
        -> err

class MissingColumn str err
  where
    missingColumn
        :: str  -- ^ Column name
        -> err

class FieldInvalidUtf8 str err
  where
    fieldInvalidUtf8
        :: str  -- ^ Column name
        -> err
