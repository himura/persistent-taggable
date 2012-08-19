{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Query.Taggable.Sql
import Database.Persist.Query.Taggable.SqlInternal
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Monad

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Language
    fullname T.Text Eq
    deriving Show
Tag
    name T.Text Update Eq
    UniqueTagNameKey name
    deriving Show
LanguageTag
    language LanguageId Eq
    tag TagId Eq
    UniqueLanguageTag language tag
    deriving Show
|]

main :: IO ()
main = withSqliteConn ":memory:" . runSqlConn $ do
    runMigration migrateAll

    haskell <- insert $ Language "Haskell"
    ocaml <- insert $ Language "OCaml"
    scala <- insert $ Language "Scala"
    java <- insert $ Language "Java"

    pure <- insert $ Tag "Pure"
    functional <- insert $ Tag "Functional"
    jvm <- insert $ Tag "JVM"
    native <- insert $ Tag "Native"

    forM_ [haskell, ocaml, scala] $ \lang -> do
        insert $ LanguageTag lang functional

    forM_ [java, scala] $ \lang -> do
        insert $ LanguageTag lang jvm

    forM_ [ocaml, haskell] $ \lang -> do
        insert $ LanguageTag lang native

    void . insert $ LanguageTag haskell pure

    let query = (taggable [functional, native]
                 (LanguageTagTag <-.)
                 (LanguageTagLanguage ==. undefined))
                { taggableFilters = [] :: [Filter Language]
                }
    C.runResourceT
        $    selectTaggableSource query
        C.$$ CL.mapM_ $ \lang -> do
            liftIO . print . entityVal $ lang

    liftIO $ putStrLn "\nQuery:"
    (sql, vals) <- makeQuery query
    liftIO $ do
        T.putStrLn sql
        print vals
