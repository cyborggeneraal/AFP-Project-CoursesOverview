{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import ApiCourses (appCourses)
import Types (dummyCourses, User(User))
import Test.Hspec
import Test.Hspec.Wai
import Data.Aeson (toJSON, Value, decode)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Data.Time

import Database
import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite
import System.Directory

main :: IO ()
main = do
  createTestDb
  hspec spec
  removeFile "test.db"

dummyUsers :: [User]
dummyUsers =
    [ User "IsaacNewton"     372 "isaac@newton.co.uk" (fromGregorian 1683 3 1) 
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

createTestDb :: IO ()
createTestDb = do
  conn <- open "test.db"

  execute_ conn "DROP TABLE IF EXISTS users;"

  createDatabase conn

  runBeamSqlite conn $ runInsert $
    insert (users userDb) $
      insertValues $ userToDbUser <$> dummyUsers

spec :: Spec
spec = do
    businessLogicSpec

withUserApp :: IO() -> IO ()
withUserApp action =
    bracket (C.forkIO $ Warp.run 9999 $ appCourses "test.db")
      C.killThread
      (const action)

businessLogicSpec :: Spec
businessLogicSpec =
    around_ withUserApp $ do
        with (pure $ appCourses "test.db") $ do
            describe "GET /courses" $ do
                it "should get dummy courses" $
                  get "/courses" `shouldRespondWith` 200 {matchBody = matchDummyCourses}
            describe "GET /courses/course_id" $ do
                it "should get a course" $
                  get "/courses/101" `shouldRespondWith` 200 {matchBody = matchCourse101}
                it "should get an empty list" $
                  get "/courses/105" `shouldRespondWith` 200 {matchBody = matchEmptyList}
            describe "GET /courses/course_id/prereq" $ do
                it "should return 101 for 201" $
                  get "/courses/201/prereq"  `shouldRespondWith` 200 {matchBody = matchCourse101}
                it "should return empty for 101" $
                  get "/courses/101/prereq" `shouldRespondWith` 200 {matchBody = matchEmptyList}
                it "should return empty for 105" $
                  get "/courses/105/prereq" `shouldRespondWith` 200 {matchBody = matchEmptyList}
            describe "GET /users" $ do
                it "should return all users" $
                  get "/users" `shouldRespondWith` 200 {matchBody = matchAllUsers}
            describe "GET /users/name" $ do
                it "should return isaac newton" $
                  get "/users/IsaacNewton" `shouldRespondWith` 200 {matchBody = matchIsaac}
                it "should return error" $
                  get "/users/Hamilton" `shouldRespondWith` 404
            describe "GET /users/name/courses" $ do
                it "should return no courses" $
                  get "/users/Albert Einstein/courses" `shouldRespondWith` 200 {matchBody = matchEmptyList}
                it "should return 101" $
                  get "/users/IsaacNewton/courses" `shouldRespondWith` 200 {matchBody = matchCourse101} 
                it "should return no courses" $
                  get "/users/Hamilton/courses" `shouldRespondWith` 200 {matchBody = matchEmptyList}

matchValue :: Value -> [a] -> Body -> Maybe String
matchValue v _ body = case (decode body :: Maybe Value) of
  Just val | val == v -> Nothing
  _                   -> Just "Error"

matchBodyValue :: Value -> MatchBody
matchBodyValue v = MatchBody $ matchValue v

matchDummyCourses :: MatchBody
matchDummyCourses = matchBodyValue (toJSON dummyCourses)

matchCourse101:: MatchBody
matchCourse101 = matchBodyValue (toJSON (take 1 dummyCourses))

matchEmptyList :: MatchBody
matchEmptyList = matchBodyValue (toJSON ([] :: [String]))

matchAllUsers :: MatchBody
matchAllUsers = matchBodyValue (toJSON dummyUsers)

matchIsaac :: MatchBody
matchIsaac = matchBodyValue (toJSON (head dummyUsers))
