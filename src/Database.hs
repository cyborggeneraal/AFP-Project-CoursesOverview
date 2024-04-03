{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Servant

import Data.Text (Text, unpack, pack)
import Data.Int (Int32)
import Data.Aeson
import Data.Swagger ( ToSchema )
import Data.Time

import Types

type DatabasePath = String

data DbUserT f
    = DBUser
    { _name :: Columnar f Text
    , _age :: Columnar f Int32
    , _email :: Columnar f Text
    , _registeration_date :: Columnar f Day }
    deriving (Generic, Beamable)

userTableQuery :: Query
userTableQuery = "CREATE TABLE users (name TEXT NOT NULL, age INT NOT NULL, email TEXT NOT NULL, registeration_date DATE NOT NULL, PRIMARY KEY(name));"

type DbUser = DbUserT Identity
type DbUserId = PrimaryKey DbUserT Identity

createDatabase :: Connection -> IO ()
createDatabase conn = do
    execute_ conn userTableQuery

deriving instance Show DbUser
deriving instance Eq DbUser
instance ToJSON DbUser
instance ToSchema DbUser

instance Table DbUserT where
    data PrimaryKey DbUserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
    primaryKey = UserId <$> _name

newtype UserDb f
    = UserDb
    { users :: f (TableEntity DbUserT) }
    deriving (Generic, Database be)

userDb :: DatabaseSettings be UserDb
userDb = defaultDbSettings

allDbUsers :: DatabasePath -> Handler [DbUser]
allDbUsers p = liftIO $ do 
    conn <- open p
    runBeamSqlite conn $ runSelectReturningList (select (all_ (users userDb)))

addDbUser :: DatabasePath -> DbUser -> Handler ()
addDbUser p u = liftIO $ do
    conn <- open p
    runBeamSqlite conn $ runInsert $
        insert (users userDb) $
        insertValues [u]

dbUserToUser :: DbUser -> User
dbUserToUser d = User (unpack $ _name d) (fromIntegral $ _age d) (unpack $ _email d) (_registeration_date d)

userToDbUser :: User -> DbUser
userToDbUser u = DBUser (pack $ name u) (fromIntegral $ age u) (pack $ email u) (registeration_date u)

allUsers :: DatabasePath -> Handler [User]
allUsers p = (dbUserToUser <$>) <$> allDbUsers p