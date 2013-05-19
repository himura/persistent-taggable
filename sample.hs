{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Query.Taggable.Sql
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
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

    let taggableField = TaggableField LanguageId LanguageTagTag LanguageTagLanguage
        query = TagQuery [functional, native] [] []
    src <- selectTaggableSource taggableField query
    C.runResourceT $ src C.$$ CL.mapM_ $ \lang -> do
        liftIO . print $ lang
