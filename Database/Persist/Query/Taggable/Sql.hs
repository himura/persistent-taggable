{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Query.Taggable.Sql
       ( Taggable(..)
       , taggable
       , selectTaggableSource
       )
       where

import Database.Persist.EntityDef
import Database.Persist.Store
import Database.Persist.Query.Internal
import Database.Persist.Query.GenericSql
import Database.Persist.GenericSql.Internal
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Database.Persist.GenericSql.Raw as R
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Control.Exception (throwIO)
import Data.Monoid (mappend)

import qualified Prelude
import Prelude hiding ((++), show)

data Taggable backend taggable tag tagmap =
  Taggable
  { taggableTags :: [Key backend tag]
  , taggableRejectTags :: [Key backend tag]
  , taggableFilters :: [Filter taggable]
  , taggableOpts :: [SelectOpt taggable]
  , taggableTagMapFilt :: [Key backend tag] -> Filter tagmap
  , taggableGetKey :: Filter tagmap
  , taggableAny :: Bool
  }

taggable :: [Key backend tag]
         -> ([Key backend tag] -> Filter tagmap)
         -> Filter tagmap
         -> Taggable backend taggable tag tagmap
taggable tags tmF getKey = Taggable tags [] [] [] tmF getKey False

selectTaggableSource :: (PersistEntityBackend tag ~ SqlPersist,
                         C.ResourceIO m,
                         PersistEntity taggable,
                         PersistEntity tag,
                         PersistEntity tagmap)
                     => Taggable SqlPersist taggable tag tagmap
                     -> C.Source (SqlPersist m) (Entity taggable)
selectTaggableSource (Taggable tags rejtags filts opts tmF getKey anyP) = C.Source
    { C.sourcePull = do
         conn <- lift $ SqlPersist ask
         let filtTagMap = tmF tags
             filtRejTagMap = tmF rejtags
             vals = getFiltsValues conn [filtTagMap] Prelude.++
                    getFiltsValues conn [filtRejTagMap] Prelude.++
                    getFiltsValues conn filts
             src = R.withStmt (sql conn) vals C.$= CL.mapM parse
         C.sourcePull src
    , C.sourceClose = return ()
    }
  where
    (limit, offset, orders) = limitOffsetOrder opts

    parse vals =
      case fromPersistValues' vals of
        Left s -> liftIO $ throwIO $ PersistMarshalError s
        Right row -> return row

    t = entityDef $ dummyFromFilts filts

    fromPersistValues' (PersistInt64 x:xs) =
      case fromPersistValues xs of
        Left e -> Left e
        Right xs' -> Right (Entity (Key $ PersistInt64 x) xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"

    wher conn =
      let rejWher = rejtagWhere conn
          s = filterClauseNoWhere True conn filts in
      case (null rejtags, T.null s) of
        (True, True) -> ""
        (False, True) -> " WHERE " ++ rejWher
        (True, False) -> " WHERE " ++ s
        (False, False) -> " WHERE (" ++ rejWher ++ ") AND (" ++ s ++ ")"

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
            ords -> " ORDER BY " ++ T.intercalate "," ords

    lim conn = case (limit, offset) of
            (0, 0) -> ""
            (0, _) -> T.cons ' ' $ noLimit conn
            (_, _) -> " LIMIT " ++ show limit

    off = if offset == 0
            then ""
            else " OFFSET " ++ show offset

    cols conn = T.intercalate ","
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
        , if anyP then "> 0" else "== " ++ (show . length $ tags)
        , ") ON "
        , tqKey
        , " = "
        , escapeName conn (entityDB t)
        , ".id"
        ]
        where tqKey = escapeName conn $ filterName getKey

    tqWhere conn =
      let s = filterClauseNoWhere False conn [tmF tags] in
      if not (T.null s)
        then " WHERE " ++ s
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

filterName :: PersistEntity v => Filter v -> DBName
filterName (Filter f _ _) = fieldDB $ persistFieldDef f
filterName (FilterAnd _) = error "expected a raw filter, not an And"
filterName (FilterOr _) = error "expected a raw filter, not an Or"

infixr 5 ++
(++) :: T.Text -> T.Text -> T.Text
(++) = mappend

show :: Show a => a -> T.Text
show = T.pack . Prelude.show
