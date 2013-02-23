{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Persist.Query.Taggable.SqlInternal
       ( Taggable(..)
       , TagQuery (..)
       , taggable
       , makeQuery
       , selectTaggableSource
       )
       where

import Database.Persist.Query
import Database.Persist.EntityDef
import Database.Persist.Store
import Database.Persist.Query.Internal
import Database.Persist.Query.GenericSql
import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

data TagQuery tag = TagQueryAnd [Key tag] | TagQueryAny [Key tag]

data Taggable taggable tag tagmap =
    Taggable
    { taggableTags :: [TagQuery tag]
    , taggableRejectTags :: [Key tag]
    , taggableFilters :: [Filter taggable]
    , taggableOpts :: [SelectOpt taggable]
    , taggableTagMapFilt :: [Key tag] -> Filter tagmap
    , taggableGetKey :: Filter tagmap
    }

taggable :: EntityField tagmap (Key tag)
         -> EntityField tagmap (Key taggable)
         -> [TagQuery tag]
         -> Taggable taggable tag tagmap
taggable tmF getKey tags = Taggable tags [] [] [] (tmF <-.) (getKey ==. undefined)

makeQuery :: ( Monad m
             , PersistEntity taggable
             , PersistEntity tag
             , PersistEntity tagmap)
          => Taggable taggable tag tagmap
          -> SqlPersist m (T.Text, [PersistValue])
makeQuery (Taggable tagsQuery rejtags filts opts tmF getKey) = do
    conn <- SqlPersist ask
    let (s, tagvals) = sql conn
        filtRejTagMap = tmF rejtags
        vals = tagvals ++
               getFiltsValues conn [filtRejTagMap] ++
               getFiltsValues conn filts
    return (s, vals)
  where
    (limit, offset, orders) = limitOffsetOrder opts

    t = entityDef $ dummyFromFilts filts

    wher conn =
        let rejWher = rejtagWhere conn
            s = filterClauseNoWhere True conn filts in
        case (null rejtags, T.null s) of
            (True, True) -> ""
            (False, True) -> " WHERE " <> rejWher
            (True, False) -> " WHERE " <> s
            (False, False) -> " WHERE (" <> rejWher <> ") AND (" <> s <> ")"

    rejtagWhere conn = T.concat
        [ escapeName conn (entityDB t)
        , ".id NOT IN (SELECT "
        , escapeName conn $ filterName getKey
        , " FROM "
        , escapeName conn $ entityDB $ entityDef $ dummyFromFilts [tmF undefined]
        , " WHERE "
        , filterClauseNoWhere False conn [tmF rejtags]
        , ")"
        ]

    ord conn =
        case map (orderClause False conn) orders of
            [] -> ""
            ords -> " ORDER BY " <> T.intercalate "," ords

    lim conn = case (limit, offset) of
        (0, 0) -> ""
        (0, _) -> T.cons ' ' $ noLimit conn
        (_, _) -> " LIMIT " <> showTxt limit

    off = if offset == 0
        then ""
        else " OFFSET " <> showTxt offset

    cols conn =
        T.intercalate ","
        $ escapeName conn (entityID t)
        : map (escapeName conn . fieldDB) (entityFields t)

    makeTagQuery _ _ ([], _) = ("", [])
    makeTagQuery conn anyP (tags, n) = (s, vals)
      where
        (wh, vals) = tqWhere conn tags
        s = T.concat
            [ " INNER JOIN (SELECT "
            , tqKey
            , " FROM "
            , escapeName conn $ entityDB $ entityDef $ dummyFromFilts [tmF undefined]
            , wh
            , " GROUP BY "
            , tqKey
            , " HAVING COUNT("
            , tqKey
            , ")"
            , if anyP then "> 0" else "= " <> (showTxt . length $ tags)
            , ") ", mtag, " ON ", mtag, "."
            , tqKey
            , " = "
            , escapeName conn (entityDB t)
            , ".id"
            ]
        tqKey = escapeName conn $ filterName getKey
        mtag = "MTAG" <> showTxt n

    tqWhere conn tags =
        if not (T.null s)
        then (" WHERE " <> s, vals)
        else ("", vals)
      where
        filt = [tmF tags]
        vals = getFiltsValues conn filt
        s = filterClauseNoWhere False conn filt

    (tagands, taganys) = foldr go ([], []) tagsQuery
      where
        go (TagQueryAnd v) ~(ands,ors) = (v:ands, ors)
        go (TagQueryAny v) ~(ands,ors) = (ands, v:ors)

    sql conn = (s, andVals ++ concat anyVals)
      where
        s = T.concat $
            [ "SELECT "
            , cols conn
            , " FROM "
            , escapeName conn $ entityDB t
            , sqland
            ]
            ++ sqlanys
            ++
            [ wher conn
            , ord conn
            , lim conn
            , off
            ]
        (sqland, andVals) = makeTagQuery conn False ((concat tagands), 0)
        (sqlanys, anyVals) = unzip $ map (makeTagQuery conn True) $ zip taganys [(1::Int)..]

selectTaggableSource :: ( C.MonadResource m
                        , MonadLogger m
                        , PersistEntity taggable
                        , PersistEntity tag
                        , PersistEntity tagmap)
                     => Taggable taggable tag tagmap
                     -> C.Source (SqlPersist m) (Entity taggable)
selectTaggableSource t = do
    (sql, vals) <- lift $ makeQuery t
    R.withStmt sql vals C.$= CL.mapM parse
  where
    parse vals =
        case fromPersistValues' vals of
            Left s -> C.monadThrow $ PersistMarshalError s
            Right row -> return row

    fromPersistValues' (PersistInt64 x:xs) =
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> Right (Entity (Key $ PersistInt64 x) xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"

filterName :: PersistEntity v => Filter v -> DBName
filterName (Filter f _ _) = fieldDB $ persistFieldDef f
filterName (FilterAnd _) = error "expected a raw filter, not an And"
filterName (FilterOr _) = error "expected a raw filter, not an Or"
filterName (BackendFilter _) = error "expected a raw filter, not an Or"

showTxt :: Show a => a -> T.Text
showTxt = T.pack . show
