{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

import Database.Persist.TH
import Database.Persist.Sqlite
import qualified Database.Esqueleto as E
import qualified Database.Persist.Query.Taggable.Sql as Taggable
import qualified Data.Conduit as C
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
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

run :: SqlPersistT (C.ResourceT (LoggingT IO)) a -> IO a
run = runStderrLoggingT .
      C.runResourceT .
      withSqliteConn ":memory:" .
      runSqlConn .
      (runMigration migrateAll >>)

main :: IO ()
main = run $ do
    let taggingField = Taggable.TaggingFieldDef LanguageId LanguageTagTag LanguageTagLanguage
        taggedWith = Taggable.taggedWith taggingField

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

    liftIO $ putStrLn "==== Functional ===="
    res1 <- E.select $ E.from $ \language -> do
        language `taggedWith` [functional]
        return language
    liftIO . mapM_ print $ res1
    liftIO $ putStrLn "==== Functional && Native ===="
    res2 <- E.select $ E.from $ \language -> do
        language `taggedWith` [functional, native]
        return language
    liftIO . mapM_ print $ res2

    liftIO $ putStrLn "==== Functional && Native ===="
    res3 <- E.select $ E.from $ \language -> do
        language `taggedWith` [functional, native]
        E.where_ $ (language E.^. LanguageFullname) E.!=. (E.val "Haskell")
        return language
    liftIO . mapM_ print $ res3
