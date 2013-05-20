{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

import Test.Hspec
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Query.Taggable.Sql
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.List as Lst
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Language
    name T.Text Eq
    deriving Show
    deriving Eq
    deriving Ord
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

prepare :: ( C.MonadResource m
           , MonadLogger m
           ) => SqlPersistT m ()
prepare = do
    haskell <- insert $ Language "Haskell"
    ocaml <- insert $ Language "OCaml"
    lisp <- insert $ Language "Lisp"
    java <- insert $ Language "Java"
    scala <- insert $ Language "Scala"
    cpp <- insert $ Language "C++"
    ruby <- insert $ Language "Ruby"

    pure <- insert $ Tag "Pure"
    forM_ [haskell] $ \lang -> insert $ LanguageTag lang pure

    functional <- insert $ Tag "Functional"
    forM_ [haskell, ocaml, lisp, scala] $ \lang -> (insert $ LanguageTag lang functional)

    strongly_typed <- insert $ Tag "StronglyTyped"
    forM_ [haskell, ocaml, java, scala, cpp, ruby] $ \lang -> (insert $ LanguageTag lang strongly_typed)

    static_typed <- insert $ Tag "StaticTyped"
    forM_ [haskell, ocaml, java, scala, cpp] $ \lang -> (insert $ LanguageTag lang static_typed)

    oop <- insert $ Tag "OOP"
    forM_ [java, ocaml, scala, cpp, ruby] $ \lang -> (insert $ LanguageTag lang oop)

    jvm <- insert $ Tag "JVM"
    forM_ [java, scala] $ \lang -> (insert $ LanguageTag lang jvm)

runTest :: SqlPersistT (C.ResourceT (LoggingT IO)) a -> IO a
runTest = run . (prepare >>)

queryTaggableVal :: TagQuery (TagGeneric SqlBackend)
                 -> SqlPersistT (C.ResourceT (LoggingT IO)) [LanguageGeneric SqlBackend]
queryTaggableVal query = do
    res <- selectTaggableSource field query
    C.runResourceT
        $    res
        C.$= CL.map entityVal
        C.$$ CL.consume
  where
    field = TaggableField LanguageId LanguageTagTag LanguageTagLanguage

main :: IO ()
main = hspec $
    describe "with database" $ do
        it "single" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Java", "Scala", "C++", "Ruby"]

            ret <- queryTaggableVal $ TagQuery [strongly] [] []
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "and query" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Java", "Scala", "C++"]

            ret <- queryTaggableVal $ TagQuery [strongly, static] [] []
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "any" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Lisp", "Java", "Scala", "C++"]
            ret <- queryTaggableVal $ TagQuery [] [[functional, static]] []
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "any3" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Lisp", "Java", "Scala", "C++", "Ruby"]
            ret <- queryTaggableVal $ TagQuery [] [[functional, static, oop]] []
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "perform both 'and' and 'any' query simultaneously" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            let ex = Lst.sort $ map Language ["OCaml", "Java", "Scala", "C++"]

            ret1 <- queryTaggableVal $ TagQuery [oop, strongly] [[functional, static]] []
            liftIO $ (Lst.sort ret1) `shouldBe` ex

            ret2 <- queryTaggableVal $ TagQuery [oop, strongly] [[functional, static]] []
            liftIO $ (Lst.sort ret2) `shouldBe` ex

            ret3 <- queryTaggableVal $ TagQuery [oop, strongly] [[functional, static]] []
            liftIO $ (Lst.sort ret3) `shouldBe` ex

        it "reject tag" $ runTest $ do
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity jvm _) <- getBy $ UniqueTagNameKey "JVM"
            Just (Entity pure _) <- getBy $ UniqueTagNameKey "Pure"
            let ex = Lst.sort $ map Language ["Haskell"]
            ret <- queryTaggableVal (TagQuery [strongly, static] [[pure, jvm]] [oop])
            liftIO $ (Lst.sort ret) `shouldBe` ex
