{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Query.Taggable.Sql
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
    ( SqlEntity taggable
    , SqlEntity tagging
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
taggableQuery = (E.from .) . taggableWhere

taggableWhere :: ( TaggableConstraint backend taggable tagging
                 , E.ToSomeValues expr (expr (Value (KeyBackend backend taggable)))
                 , E.From query expr backend (expr (Entity tagging))
                 )
              => TaggableField taggable tag tagging
              -> TagQuery tag
              -> expr (Entity taggable)
              -> query (expr (Value (Key taggable)))
taggableWhere tf@TaggableField{..} TagQuery{..} taggable = do
    E.where_ andQuery
    when (not . null $ tagQueryAnyTags) $
        E.where_ $ foldr1 (E.&&.) $ map (anyQuery taggable) tagQueryAnyTags
    return (taggable E.^. taggable_taggableId)
  where
    andQuery = makeTagQuery tf (E.==. (E.val (length tagQueryTags))) taggable tagQueryTags
    anyQuery = makeTagQuery tf (E.>. (E.val (0 :: Int)))

taggableJoin :: ( SqlEntity tagging
                , SqlEntity taggable
                , E.ToSomeValues expr (expr (Value (KeyBackend SqlBackend taggable)))
                , E.FromPreprocess query expr SqlBackend (expr (Entity taggable))
                , E.From query expr SqlBackend (expr (Entity tagging))
                )
             => TaggableField taggable tag tagging
             -> TagQuery tag
             -> query (expr (Entity taggable))
taggableJoin tf@TaggableField{..} tq@TagQuery{..} = do
    E.from $ \(taggable `E.InnerJoin` matched) -> do
        E.on $ taggable E.^. taggable_taggableId E.==. matched E.^. taggable_taggableId
        _taggableKey <- taggableWhere tf tq matched
        -- when (not . null $ tagQueryRejectTags) $
        --     E.where_ $ (taggable E.^. taggable_taggingTagId) `E.notIn` (E.valList tagQueryRejectTags)
        return $ taggable

selectTaggable :: ( SqlEntity tagging
                  , SqlEntity taggable
                  , RunDbMonad m
                  )
               => TaggableField taggable tag tagging
               -> TagQuery tag
               -> SqlPersistT m [Entity taggable]
selectTaggable = (E.select .) . taggableJoin

selectTaggableKey
    :: ( RunDbMonad m, TaggableConstraint backend taggable tagging)
    => TaggableField taggable tag tagging
    -> TagQuery tag
    -> SqlPersistT m [Value (KeyBackend backend taggable)]
selectTaggableKey = (E.select .) . taggableQuery

selectTaggableSource
    :: ( RunDbMonad m, TaggableConstraint backend taggable tagging)
    => TaggableField taggable tag tagging
    -> TagQuery tag
    -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) (Entity taggable))
selectTaggableSource = (E.selectSource .) . taggableJoin

selectTaggableKeySource
    :: ( RunDbMonad m, TaggableConstraint backend taggable tagging)
    => TaggableField taggable tag tagging
    -> TagQuery tag
    -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) (Value (KeyBackend SqlBackend taggable)))
selectTaggableKeySource = (E.selectSource .) . taggableQuery

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

