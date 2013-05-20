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

taggableQuery :: ( SqlEntity tagging
                 , SqlEntity taggable
                 , E.ToSomeValues expr (expr (Value (KeyBackend SqlBackend taggable)))
                 , E.From query expr SqlBackend (expr (Entity tagging))
                 , E.From query expr SqlBackend (expr (Entity taggable))
                 ) => TaggableField taggable tag tagging
              -> TagQuery tag
              -> query (expr (Entity taggable))
taggableQuery tf@TaggableField{..} TagQuery{..} =
    E.from $ \taggable -> do
        when (not . null $ tagQueryTags) $
            E.where_ $ andQuery taggable tagQueryTags
        when (not . null $ tagQueryAnyTags) $
            E.where_ $ foldr1 (E.&&.) $ map (anyQuery taggable) tagQueryAnyTags
        when (not . null $ tagQueryRejectTags) $
            E.where_ $ E.notIn (taggable E.^. taggable_taggableId) $
                E.subList_select $
                E.from $ \rejtags -> do
                   E.where_ $ rejtags E.^. taggable_taggingTagId `E.in_` E.valList tagQueryRejectTags
                   return $ rejtags E.^. taggable_taggingTaggableId
        return taggable
  where
    andQuery = makeTagQuery tf (E.==. (E.val (length tagQueryTags)))
    anyQuery = makeTagQuery tf (E.>. (E.val (0 :: Int)))

selectTaggable :: ( SqlEntity tagging
                  , SqlEntity taggable
                  , RunDbMonad m
                  )
               => TaggableField taggable tag tagging
               -> TagQuery tag
               -> SqlPersistT m [Entity taggable]
selectTaggable = (E.select .) . taggableQuery

selectTaggableSource
    :: ( RunDbMonad m
       , SqlEntity taggable
       , SqlEntity tagging
       )
    => TaggableField taggable tag tagging
    -> TagQuery tag
    -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) (Entity taggable))
selectTaggableSource = (E.selectSource .) . taggableQuery

makeTagQuery
    :: ( SqlEntity taggable
       , SqlEntity tagging
       , E.ToSomeValues expr (expr (Value (KeyBackend SqlBackend taggable)))
       , E.From query expr SqlBackend (expr (Entity tagging))
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

