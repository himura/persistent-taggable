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

data TagQuery expr taggable tag tagging =
    TagQuery
    { tagQueryTags :: [Key tag]
    , tagQueryAnyTags :: [[Key tag]]
    , tagQueryRejectTags :: [Key tag]
    , tagQueryWhere :: Maybe (expr (Entity taggable) -> expr (Value Bool))
    , tagQueryFieldDef :: TagQueryFieldDef taggable tag tagging
    }

data TagQueryFieldDef taggable tag tagging =
    TagQueryFieldDef
    { tagQueryTaggableId :: EntityField taggable (Key taggable)
    , tagQueryTaggingTagId :: EntityField tagging (Key tag)
    , tagQueryTaggingTaggableId :: EntityField tagging (Key taggable)
    }

tagQuery :: TagQueryFieldDef taggable tag tagging
         -> TagQuery expr taggable tag tagging
tagQuery = TagQuery [] [] [] Nothing

type RunDbMonad m = ( C.MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , C.MonadUnsafeIO m, C.MonadThrow m )

taggableQuery :: ( SqlEntity tagging
                 , SqlEntity taggable
                 , E.ToSomeValues expr (expr (Value (KeyBackend SqlBackend taggable)))
                 , E.From query expr SqlBackend (expr (Entity tagging))
                 , E.From query expr SqlBackend (expr (Entity taggable))
                 )
              => TagQuery expr taggable tag tagging
              -> query (expr (Entity taggable))
taggableQuery TagQuery{..} =
    E.from $ \taggable -> do
        when (not . null $ tagQueryTags) $
            E.where_ $ andQuery taggable tagQueryTags
        when (not . null $ tagQueryAnyTags) $
            E.where_ $ foldr1 (E.&&.) $ map (anyQuery taggable) tagQueryAnyTags
        when (not . null $ tagQueryRejectTags) $
            E.where_ $ E.notIn (taggable E.^. tagQueryTaggableId) $
                E.subList_select $
                E.from $ \rejtags -> do
                   E.where_ $ rejtags E.^. tagQueryTaggingTagId `E.in_` E.valList tagQueryRejectTags
                   return $ rejtags E.^. tagQueryTaggingTaggableId
        case tagQueryWhere of
            Just query -> E.where_ (query taggable)
            Nothing -> return ()
        return taggable
  where
    TagQueryFieldDef{..} = tagQueryFieldDef
    andQuery = makeTagQuery tagQueryFieldDef (E.==. (E.val (length tagQueryTags)))
    anyQuery = makeTagQuery tagQueryFieldDef (E.>. (E.val (0 :: Int)))

selectTaggable :: ( SqlEntity taggable
                  , SqlEntity tagging
                  , RunDbMonad m
                  )
               => TagQuery SqlExpr taggable tag tagging
               -> SqlPersistT m [Entity taggable]
selectTaggable = E.select . taggableQuery

selectTaggableSource
    :: ( RunDbMonad m
       , SqlEntity taggable
       , SqlEntity tagging
       )
    => TagQuery SqlExpr taggable tag tagging
    -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) (Entity taggable))
selectTaggableSource = E.selectSource . taggableQuery

makeTagQuery
    :: ( SqlEntity taggable
       , SqlEntity tagging
       , E.ToSomeValues expr (expr (Value (KeyBackend SqlBackend taggable)))
       , E.From query expr SqlBackend (expr (Entity tagging))
       )
    => TagQueryFieldDef taggable tag tagging
    -> (expr (Value Int) -> expr (Value Bool))
    -> expr (Entity taggable) -> [Key tag] -> expr (Value Bool)
makeTagQuery TagQueryFieldDef{..} condition taggable tags =
    E.in_ (taggable E.^. tagQueryTaggableId) $
    E.subList_select $
    E.from $ \tagging -> do
        E.where_ (tagging E.^. tagQueryTaggingTagId `E.in_` (E.valList tags))
        E.groupBy (tagging E.^. tagQueryTaggingTaggableId)
        let cnt = E.count (tagging E.^. tagQueryTaggingTaggableId)
        E.having (condition cnt)
        return (tagging E.^. tagQueryTaggingTaggableId)


