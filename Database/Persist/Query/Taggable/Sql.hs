{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Query.Taggable.Sql
       where

import Control.Lens
import qualified Database.Esqueleto.Internal.Language as E
import Database.Esqueleto as E
import qualified Data.Conduit as C
import Control.Monad.Logger
import Control.Monad
import Control.Monad.IO.Class

data TagQueryFieldDef taggable tag tagging =
    TagQueryFieldDef
    { _taggableId :: EntityField taggable (Key taggable)
    , _taggingTagId :: EntityField tagging (Key tag)
    , _taggingTaggableId :: EntityField tagging (Key taggable)
    }
makeLenses ''TagQueryFieldDef

data TagQuery query expr taggable tag tagging =
    TagQuery
    { _tags :: [Key tag]
    , _anyTags :: [[Key tag]]
    , _rejectTags :: [Key tag]
    , _additional :: (expr (Entity taggable) -> query ())
    , _fieldDef :: TagQueryFieldDef taggable tag tagging
    }
makeLenses ''TagQuery

tagQuery :: Monad query
         => TagQueryFieldDef taggable tag tagging
         -> TagQuery query expr taggable tag tagging
tagQuery = TagQuery [] [] [] (const (return ()))

type RunDbMonad m = ( C.MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , C.MonadUnsafeIO m, C.MonadThrow m )

taggableQuery :: ( SqlEntity tagging
                 , SqlEntity taggable
                 , E.ToSomeValues expr (expr (Value (KeyBackend SqlBackend taggable)))
                 , E.From query expr SqlBackend (expr (Entity tagging))
                 , E.From query expr SqlBackend (expr (Entity taggable))
                 )
              => TagQuery query expr taggable tag tagging
              -> query (expr (Entity taggable))
taggableQuery TagQuery{..} =
    E.from $ \taggable -> do
        when (not . null $ _tags) $
            E.where_ $ andQuery taggable _tags
        when (not . null $ _anyTags) $
            E.where_ $ foldr1 (E.&&.) $ map (anyQuery taggable) _anyTags
        when (not . null $ _rejectTags) $
            E.where_ $ E.notIn (taggable E.^. _taggableId) $
                E.subList_select $
                E.from $ \rejtags -> do
                   E.where_ $ rejtags E.^. _taggingTagId `E.in_` E.valList _rejectTags
                   return $ rejtags E.^. _taggingTaggableId
        _additional taggable
        return taggable
  where
    TagQueryFieldDef{..} = _fieldDef
    andQuery = makeTagQuery _fieldDef (E.==. (E.val (length _tags)))
    anyQuery = makeTagQuery _fieldDef (E.>. (E.val (0 :: Int)))

selectTaggable :: ( SqlEntity taggable
                  , SqlEntity tagging
                  , RunDbMonad m
                  )
               => TagQuery SqlQuery SqlExpr taggable tag tagging
               -> SqlPersistT m [Entity taggable]
selectTaggable = E.select . taggableQuery

selectTaggableSource
    :: ( RunDbMonad m
       , SqlEntity taggable
       , SqlEntity tagging
       )
    => TagQuery SqlQuery SqlExpr taggable tag tagging
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
makeTagQuery TagQueryFieldDef{..} condition taggable tagList =
    E.in_ (taggable E.^. _taggableId) $
    E.subList_select $
    E.from $ \tagging -> do
        E.where_ (tagging E.^. _taggingTagId `E.in_` (E.valList tagList))
        E.groupBy (tagging E.^. _taggingTaggableId)
        let cnt = E.count (tagging E.^. _taggingTaggableId)
        E.having (condition cnt)
        return (tagging E.^. _taggingTaggableId)


