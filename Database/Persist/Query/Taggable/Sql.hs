{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Query.Taggable.Sql
       where

import Database.Esqueleto.Internal.Language as E
import Database.Esqueleto as E
import qualified Control.Lens as Lens
import Control.Monad

type Tagging query expr taggable tagging =
    ( PersistEntity taggable
    , PersistEntity tagging
    , ToSomeValues expr (expr (Value (KeyBackend (PersistEntityBackend taggable) taggable)))
    , From query expr (PersistEntityBackend taggable) (expr (Entity tagging))
    )

data TaggingFieldDef taggable tag tagging =
    TaggingFieldDef
    { taggableId :: EntityField taggable (Key taggable)
    , taggingTagId :: EntityField tagging (Key tag)
    , taggingTaggableId :: EntityField tagging (Key taggable)
    }

data TagQuery tag =
    TagQuery
    { _tags :: [Key tag]
    , _anyTags :: [[Key tag]]
    , _rejectTags :: [Key tag]
    }
Lens.makeLenses ''TagQuery

-- tagQuery :: TaggingFieldDef taggable tag tagging
--          -> TagQuery tag
--          -> (expr (Entity taggable) -> query a)
--          -> joins -> query a
-- tagQuery fd@TaggingFieldDef{..} query innerQuery =
--     go (_tags query) (_anyTags query)
--   where
--     go :: [Key tag] -> [[Key tag]] -> joins -> query a
--     go [] [] = innerQuery

baseQuery :: From query expr backend (expr (Entity taggable))
          => (expr (Entity taggable) -> query a)
          -> query (expr (Entity taggable), a)
baseQuery query =
    from $ \taggable -> do
        ret <- query taggable
        return (taggable, ret)

taggedWith, taggedWithAny
    :: Tagging query expr taggable tagging
    => TaggingFieldDef taggable tag tagging
    -> [Key tag]
    -> (TaggingFieldDef taggable tag tagging -> query (expr (Entity taggable), t))
    -> query (expr (Entity taggable), t)
taggedWith fd@TaggingFieldDef{..} tags query =
    from $ \tagging -> do
        (taggable, ret) <- query fd
        taggedWith' fd taggable tagging tags
        return (taggable, ret)
taggedWithAny fd@TaggingFieldDef{..} tags query =
    from $ \tagging -> do
        (taggable, ret) <- query fd
        taggedWithAny' fd taggable tagging tags
        return (taggable, ret)

-- test fd tags = select $ doTagQuery $ taggedWith fd tags . baseQuery fd
-- test fd tags = taggedWith fd tags >=> baseQuery fd

taggedWith', taggedWithAny'
    :: Tagging query expr taggable tagging
    => TaggingFieldDef taggable tag tagging
    -> expr (Entity taggable)
    -> expr (Entity tagging)
    -> [Key tag]
    -> query ()
taggedWith' fieldDef taggable tagging tags = makeTagQuery fieldDef (==. (val (length tags))) taggable tagging tags
taggedWithAny' fieldDef = makeTagQuery fieldDef (>. (val (0 :: Int)))

notTaggedWith'
    :: Tagging query expr taggable tagging
    => TaggingFieldDef taggable tag tagging
    -> expr (Entity taggable)
    -> [Key tag]
    -> expr (Value Bool)
notTaggedWith' TaggingFieldDef{..} taggable tags =
    notIn (taggable E.^. taggableId) $
        subList_select $
        E.from $ \rejtags -> do
            where_ $ rejtags E.^. taggingTagId `in_` valList tags
            return $ rejtags E.^. taggingTaggableId

makeTagQuery
    :: (Tagging query expr taggable tagging)
    => TaggingFieldDef taggable tag tagging
    -> (expr (Value Int) -> expr (Value Bool))
    -> expr (Entity taggable) -> expr (Entity tagging) -> [Key tag] -> query ()
makeTagQuery TaggingFieldDef{..} condition taggable tagging tagList = do
    E.where_ $ taggable E.^. taggableId ==. tagging E.^. taggingTaggableId
    E.where_ (tagging E.^. taggingTagId `in_` (valList tagList))
    E.groupBy (tagging E.^. taggingTaggableId)
    let cnt = count (tagging E.^. taggingTaggableId)
    E.having (condition cnt)
