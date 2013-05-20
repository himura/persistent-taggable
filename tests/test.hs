{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

import Test.Hspec
import Control.Lens
import Database.Esqueleto as E
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
runTest :: SqlPersistT (C.ResourceT (LoggingT IO)) a -> IO a
runTest = run . (prepare >>)

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

type TQ query expr backend = TagQuery query expr (LanguageGeneric backend) (TagGeneric backend) (LanguageTagGeneric backend)
languageQuery :: TQ SqlQuery expr backend
languageQuery = tagQuery field
  where
    field = TagQueryFieldDef LanguageId LanguageTagTag LanguageTagLanguage

queryTaggableVal :: TQ SqlQuery SqlExpr SqlBackend
                 -> SqlPersistT (C.ResourceT (LoggingT IO)) [LanguageGeneric SqlBackend]
queryTaggableVal query = do
    res <- selectTaggableSource query
    C.runResourceT
        $    res
        C.$= CL.map entityVal
        C.$$ CL.consume

main :: IO ()
main = hspec $
    describe "with database" $ do
        it "single" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Java", "Scala", "C++", "Ruby"]

            ret <- queryTaggableVal $ languageQuery & tags .~ [strongly]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "and query" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Java", "Scala", "C++"]

            ret <- queryTaggableVal $ languageQuery & tags .~ [strongly, static]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "any" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Lisp", "Java", "Scala", "C++"]
            ret <- queryTaggableVal $ languageQuery & anyTags .~ [[functional, static]]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "any3" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Lisp", "Java", "Scala", "C++", "Ruby"]
            ret <- queryTaggableVal $ languageQuery & anyTags .~ [[functional, static, oop]]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "perform both 'and' and 'any' query simultaneously" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            let ex = Lst.sort $ map Language ["OCaml", "Java", "Scala", "C++"]

            ret1 <- queryTaggableVal $ languageQuery & tags .~ [oop, strongly]
                                                     & anyTags .~ [[functional, static]]
            liftIO $ (Lst.sort ret1) `shouldBe` ex

        it "reject tag" $ runTest $ do
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity jvm _) <- getBy $ UniqueTagNameKey "JVM"
            Just (Entity pure _) <- getBy $ UniqueTagNameKey "Pure"
            let ex = Lst.sort $ map Language ["Haskell"]
            ret <- queryTaggableVal $ languageQuery & tags .~ [strongly, static]
                                                    & anyTags .~ [[pure, jvm]]
                                                    & rejectTags .~ [oop]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "addtional query" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = map Language ["C++", "Java", "OCaml", "Scala"]
                notHaskell order language = do
                    E.where_ $ language E.^. LanguageName E.!=. E.val "Haskell"
                    E.orderBy $ [order (language E.^. LanguageName)]

            ret <- queryTaggableVal $
                   languageQuery & tags .~ [strongly, static]
                                 & additional .~ (notHaskell E.asc)
            liftIO $ ret `shouldBe` ex

            retReverse <- queryTaggableVal $
                   languageQuery & tags .~ [strongly, static]
                                 & additional .~ (notHaskell E.desc)
            liftIO $ retReverse `shouldBe` (reverse ex)
