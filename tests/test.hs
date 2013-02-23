{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

import Test.Hspec.Monadic
import Test.Hspec.HUnit()
import Test.Hspec.Expectations

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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
User
    name T.Text Eq
    deriving Show
    deriving Eq
    deriving Ord
Tag
    name T.Text Update Eq
    UniqueTagNameKey name
    deriving Show
UserTag
    user UserId Eq
    tag TagId Eq
    UniqueUserTag user tag
    deriving Show
|]

tagQuery :: [TagQuery (TagGeneric backend)]
         -> Taggable (UserGeneric backend) (TagGeneric backend) (UserTagGeneric backend)
tagQuery = taggable UserTagTag UserTagUser

withDB
  :: SqlPersist (C.ResourceT IO) a -> IO a
withDB job = C.runResourceT $  withSqliteConn ":memory:" . runSqlConn $ do
    runMigration migrateAll
    job

prepare :: SqlPersist (C.ResourceT IO) (Key Tag, Key Tag)
prepare = do
    smith <- insert $ User "Smith"
    yamada <- insert $ User "Yamada"
    suzuki <- insert $ User "Suzuki"
    t1 <- insert $ Tag "TestTag1"
    t2 <- insert $ Tag "TestTag2"

    forM_ [yamada, suzuki] $ \u -> do
        insert $ UserTag u t1
    forM_ [smith, suzuki] $ \u -> do
        insert $ UserTag u t2

    return (t1, t2)

queryTaggableVal :: Taggable User Tag UserTag -> SqlPersist (C.ResourceT IO) [User]
queryTaggableVal query =
    selectTaggableSource query
    C.$= CL.map entityVal
    C.$$ CL.consume

main :: IO ()
main = hspec $
    describe "with database" $ do
        it "single" $ withDB $ do
            (t1, _t2) <- prepare
            ret <- queryTaggableVal $ tagQuery [TagQueryAnd [t1]]
            let ex = Lst.sort [User "Suzuki", User "Yamada"]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "and query" $ withDB $ do
            (t1, t2) <- prepare
            ret <- queryTaggableVal $ tagQuery [TagQueryAnd [t1, t2]]
            liftIO $ ret `shouldBe` [User "Suzuki"]

        it "and query" $ withDB $ do
            (t1, t2) <- prepare
            ret <- queryTaggableVal $ tagQuery [TagQueryAnd [t1], TagQueryAnd [t2]]
            liftIO $ ret `shouldBe` [User "Suzuki"]

        it "any" $ withDB $ do
            (t1, t2) <- prepare
            ret <- queryTaggableVal $ tagQuery [TagQueryAny [t1, t2]]
            let ex = Lst.sort [User "Suzuki", User "Yamada", User "Smith"]
            liftIO $ (Lst.sort ret) `shouldBe` ex

        it "reject tag" $ withDB $ do
            (t1, t2) <- prepare
            ret <- queryTaggableVal $ (tagQuery [TagQueryAnd [t1]]) { taggableRejectTags = [t2] }
            let ex = Lst.sort [User "Yamada"]
            liftIO $ (Lst.sort ret) `shouldBe` ex
