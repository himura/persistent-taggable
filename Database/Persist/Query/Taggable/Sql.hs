{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Query.Taggable.Sql
       ( TagQuery (..)
       , TaggableField (..)
       , selectTaggable
       , selectTaggableSource
       )
       where

import qualified Database.Esqueleto.Internal.Language as E
import Database.Esqueleto as E
import qualified Data.Conduit as C
import Control.Monad.Logger
import Control.Monad
import Control.Monad.IO.Class

data TagQuery tag =
    TagQuery
    { tagQueryTags :: [Key tag]
    , tagQueryAnyTags :: [[Key tag]]
    , tagQueryRejectTags :: [Key tag]
    }

data TaggableField taggable tag tagging =
    TaggableField
    { taggable_taggableId :: EntityField taggable (Key taggable)
    , taggable_taggingTagId :: EntityField tagging (Key tag)
    , taggable_taggingTaggableId :: EntityField tagging (Key taggable)
    }

type RunDbMonad m = ( C.MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , C.MonadUnsafeIO m, C.MonadThrow m )

type TaggableConstraint b taggable tagging =
    ( PersistEntity taggable
    , PersistEntity tagging
    , PersistEntityBackend taggable ~ b
    , PersistEntityBackend tagging ~ b
    , b ~ SqlBackend
    )

taggableQuery :: ( TaggableConstraint backend taggable tagging
                 , E.ToSomeValues expr (expr (Value (KeyBackend backend taggable)))
                 , E.From query expr backend (expr (Entity taggable))
                 , E.From query expr backend (expr (Entity tagging))
                 )
              => TaggableField taggable tag tagging
              -> TagQuery tag
              -> query (expr (Value (Key taggable)))
taggableQuery tf@TaggableField{..} TagQuery{..} =
    E.from $ \taggable -> do
    E.where_ $ andQuery taggable
    when (not . null $ tagQueryAnyTags) $
        E.where_ $ foldr1 (E.&&.) $ map (anyQuery taggable) tagQueryAnyTags
    return (taggable E.^. taggable_taggableId)
  where
    andQuery taggable = makeTagQuery tf (E.==. (E.val (length tagQueryTags))) taggable tagQueryTags
    anyQuery = makeTagQuery tf (E.>. (E.val (0 :: Int)))


selectTaggable
    :: ( RunDbMonad m, TaggableConstraint backend taggable tagging)
    => TaggableField taggable tag tagging
    -> TagQuery tag
    -> SqlPersistT m [Value (KeyBackend backend taggable)]
selectTaggable = (E.select .) . taggableQuery

selectTaggableSource
    :: ( RunDbMonad m, TaggableConstraint backend taggable tagging)
    => TaggableField taggable tag tagging
    -> TagQuery tag
    -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) (Value (KeyBackend SqlBackend taggable)))
selectTaggableSource = (E.selectSource .) . taggableQuery

makeTagQuery
    :: ( TaggableConstraint backend taggable tagging
       , E.ToSomeValues expr (expr (Value (KeyBackend backend taggable)))
       , E.From query expr backend (expr (Entity tagging))
       )
    => TaggableField taggable tag tagging
    -> (expr (Value Int) -> expr (Value Bool))
    -> expr (Entity taggable) -> [Key tag] -> expr (Value Bool)
makeTagQuery TaggableField{..} condition taggable tags =
    E.in_ (taggable E.^. taggable_taggableId) $
    E.subList_select $
    E.from $ \tagging -> do
        E.where_ (tagging E.^. taggable_taggingTagId `E.in_` (E.valList tags))
        E.groupBy (tagging E.^. taggable_taggingTaggableId)
        let cnt = E.count (tagging E.^. taggable_taggingTaggableId)
        E.having (condition cnt)
        return (tagging E.^. taggable_taggingTaggableId)
