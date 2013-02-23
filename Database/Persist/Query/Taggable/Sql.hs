{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Query.Taggable.Sql
       ( Taggable(..)
       , TagQuery (..)
       , taggable
       , selectTaggableSource
       )
       where

import Database.Persist.Query.Taggable.SqlInternal
