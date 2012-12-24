{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Query.Taggable.SqlInternal
       ( Taggable(..)
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
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Applicative

data Taggable taggable tag tagmap =
    Taggable
    { taggableTags :: [Key tag]
    , taggableRejectTags :: [Key tag]
    , taggableFilters :: [Filter taggable]
    , taggableOpts :: [SelectOpt taggable]
    , taggableTagMapFilt :: [Key tag] -> Filter tagmap
    , taggableGetKey :: Filter tagmap
    , taggableAny :: Bool
    }

taggable :: EntityField tagmap (Key tag)
         -> EntityField tagmap (Key taggable)
         -> [Key tag]
         -> Taggable taggable tag tagmap
taggable tmF getKey tags = Taggable tags [] [] [] (tmF <-.) (getKey ==. undefined) False

makeQuery :: ( C.MonadUnsafeIO m
             , C.MonadThrow m
             , MonadLogger m
             , MonadIO m
             , Applicative m
             , PersistEntity taggable
             , PersistEntity tag
             , PersistEntity tagmap)
          => Taggable taggable tag tagmap
          -> SqlPersist m (T.Text, [PersistValue])
makeQuery (Taggable tags rejtags filts opts tmF getKey anyP) = do
    conn <- SqlPersist ask
    let filtTagMap = tmF tags
        filtRejTagMap = tmF rejtags
        vals = getFiltsValues conn [filtTagMap] ++
               getFiltsValues conn [filtRejTagMap] ++
               getFiltsValues conn filts
    return (sql conn, vals)
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

    tagQuery conn = T.concat
        [ " INNER JOIN (SELECT "
        , tqKey
        , " FROM "
        , escapeName conn $ entityDB $ entityDef $ dummyFromFilts [tmF undefined]
        , tqWhere conn
        , " GROUP BY "
        , tqKey
        , " HAVING COUNT("
        , tqKey
        , ")"
        , if anyP then "> 0" else "= " <> (showTxt . length $ tags)
        , ") MTAG ON MTAG."
        , tqKey
        , " = "
        , escapeName conn (entityDB t)
        , ".id"
        ]
        where tqKey = escapeName conn $ filterName getKey

    tqWhere conn =
        let s = filterClauseNoWhere False conn [tmF tags] in
        if not (T.null s)
        then " WHERE " <> s
        else ""

    sql conn = T.concat
        [ "SELECT "
        , cols conn
        , " FROM "
        , escapeName conn $ entityDB t
        , tagQuery conn
        , wher conn
        , ord conn
        , lim conn
        , off
        ]

selectTaggableSource :: ( C.MonadUnsafeIO m
                        , C.MonadThrow m
                        , MonadLogger m
                        , MonadIO m
                        , Applicative m
                        , PersistEntity taggable
                        , PersistEntity tag
                        , PersistEntity tagmap)
                     => Taggable taggable tag tagmap
                     -> C.Source (C.ResourceT (SqlPersist m)) (Entity taggable)
selectTaggableSource t = do
    (sql, vals) <- lift . lift $ makeQuery t
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
