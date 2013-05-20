{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Database.Persist.TH
import Database.Persist.Sqlite
import qualified Database.Esqueleto as E
import Database.Persist.Query.Taggable.Sql
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

    let field = TagQueryFieldDef LanguageId LanguageTagTag LanguageTagLanguage

    liftIO $ putStrLn "==== Functional ===="
    let q1 = tagQuery field & tags .~ [functional]
    liftIO . mapM_ print =<< selectTaggable q1

    liftIO $ putStrLn "==== Functional && Native ===="
    let q2 = q1 & tags %~ cons native
    liftIO . mapM_ print =<< selectTaggable q2

    liftIO $ putStrLn "==== Functional && Native && not Haskell ===="
    let notHaskellWhere language = E.where_ $ (language E.^. LanguageFullname) E.!=. (E.val "Haskell")
        q3 = q2 & additional .~ notHaskellWhere
    liftIO . mapM_ print =<< selectTaggable q3
