{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}

import Test.Hspec
import Database.Esqueleto as E
import Database.Persist.TH
import Database.Persist.Sqlite
import qualified Database.Persist.Query.Taggable.Sql as Taggable
import qualified Data.Conduit as C
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

field :: Taggable.TaggingFieldDef (LanguageGeneric backend) (TagGeneric backend) (LanguageTagGeneric backend)
field = Taggable.TaggingFieldDef LanguageId LanguageTagTag LanguageTagLanguage

taggedWith, taggedWithAny, notTaggedWith
    :: SqlExpr (Entity (LanguageGeneric SqlBackend))
    -> [Key (TagGeneric SqlBackend)]
    -> SqlQuery ()
taggedWith = Taggable.taggedWith field
taggedWithAny = Taggable.taggedWithAny field
notTaggedWith = Taggable.notTaggedWith field

query :: SqlEntity a
      => (SqlExpr (Entity a) -> SqlQuery ())
      -> SqlPersistT (C.ResourceT (LoggingT IO)) [a]
query q = do
    res <- E.select $ E.from $ \lang -> do
        q lang
        return lang
    return $ map entityVal res

main :: IO ()
main = hspec $
    describe "with database" $ do
        it "single" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Java", "Scala", "C++", "Ruby"]

            ret <- query (`taggedWith` [strongly])

            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "and query" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Java", "Scala", "C++"]

            ret <- query (`taggedWith` [strongly, static])
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "any" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Lisp", "Java", "Scala", "C++"]
            ret <- query (`taggedWithAny` [functional, static])
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "any3" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            let ex = Lst.sort $ map Language ["Haskell", "OCaml", "Lisp", "Java", "Scala", "C++", "Ruby"]
            ret <- query (`taggedWithAny` [functional, static, oop])
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "perform both 'and' and 'any' query simultaneously" $ runTest $ do
            Just (Entity functional _) <- getBy $ UniqueTagNameKey "Functional"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            let ex = Lst.sort $ map Language ["OCaml", "Java", "Scala", "C++"]

            ret1 <- query $ \language -> do
                language `taggedWith` [oop, strongly]
                language `taggedWithAny` [functional, static]
            liftIO $ (Lst.sort ret1) `shouldBe` ex

        it "reject tag" $ runTest $ do
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            Just (Entity oop _) <- getBy $ UniqueTagNameKey "OOP"
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity jvm _) <- getBy $ UniqueTagNameKey "JVM"
            Just (Entity pure _) <- getBy $ UniqueTagNameKey "Pure"
            let ex = Lst.sort $ map Language ["Haskell"]
            ret <- query $ \language -> do
                language `taggedWith` [strongly, static]
                language `taggedWithAny` [pure, jvm]
                language `notTaggedWith` [oop]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "addtional query" $ runTest $ do
            Just (Entity strongly _) <- getBy $ UniqueTagNameKey "StronglyTyped"
            Just (Entity static _) <- getBy $ UniqueTagNameKey "StaticTyped"
            let ex = map Language ["C++", "Java", "OCaml", "Scala"]

            ret <- query $ \language -> do
                language `taggedWith` [strongly, static]
                E.where_ $ language E.^. LanguageName E.!=. E.val "Haskell"
                E.orderBy $ [E.asc (language E.^. LanguageName)]

            liftIO $ ret `shouldBe` ex

            retReverse <- query $ \language -> do
                language `taggedWith` [strongly, static]
                E.where_ $ language E.^. LanguageName E.!=. E.val "Haskell"
                E.orderBy $ [E.desc (language E.^. LanguageName)]
            liftIO $ retReverse `shouldBe` (reverse ex)
