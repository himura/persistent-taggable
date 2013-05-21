{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Query.Taggable.Sql
       where

import Database.Esqueleto.Internal.Language
import Database.Esqueleto

data TaggingFieldDef taggable tag tagging =
    TaggingFieldDef
    { taggableId :: EntityField taggable (Key taggable)
    , taggingTagId :: EntityField tagging (Key tag)
    , taggingTaggableId :: EntityField tagging (Key taggable)
    }

type Tagging query expr taggable tagging =
    ( PersistEntity taggable
    , PersistEntity tagging
    , ToSomeValues expr (expr (Value (KeyBackend (PersistEntityBackend taggable) taggable)))
    , From query expr (PersistEntityBackend taggable) (expr (Entity tagging))
    )

taggedWith, taggedWithAny, notTaggedWith
    :: Tagging query expr taggable tagging
    => TaggingFieldDef taggable tag tagging
    -> expr (Entity taggable)
    -> [Key tag]
    -> query ()
taggedWith fieldDef taggable tags = where_ $ taggedWith' fieldDef taggable tags
taggedWithAny fieldDef taggable tags = where_ $ taggedWithAny' fieldDef taggable tags
notTaggedWith fieldDef taggable tags = where_ $ notTaggedWith' fieldDef taggable tags


taggedWith', taggedWithAny', notTaggedWith'
    :: Tagging query expr taggable tagging
    => TaggingFieldDef taggable tag tagging
    -> expr (Entity taggable)
    -> [Key tag]
    -> expr (Value Bool)
taggedWith' fieldDef taggable tags = makeTagQuery fieldDef (==. (val (length tags))) taggable tags
taggedWithAny' fieldDef = makeTagQuery fieldDef (>. (val (0 :: Int)))
notTaggedWith' TaggingFieldDef{..} taggable tags =
    notIn (taggable ^. taggableId) $
        subList_select $
        from $ \rejtags -> do
            where_ $ rejtags ^. taggingTagId `in_` valList tags
            return $ rejtags ^. taggingTaggableId

makeTagQuery
    :: Tagging query expr taggable tagging
    => TaggingFieldDef taggable tag tagging
    -> (expr (Value Int) -> expr (Value Bool))
    -> expr (Entity taggable) -> [Key tag] -> expr (Value Bool)
makeTagQuery TaggingFieldDef{..} condition taggable tagList =
    in_ (taggable ^. taggableId) $
    subList_select $
    from $ \tagging -> do
        where_ (tagging ^. taggingTagId `in_` (valList tagList))
        groupBy (tagging ^. taggingTaggableId)
        let cnt = count (tagging ^. taggingTaggableId)
        having (condition cnt)
        return (tagging ^. taggingTaggableId)


