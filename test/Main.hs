{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ApiCourses (appCourses)
import Types (dummyCourses, users)
import Test.Hspec
import Test.Hspec.Wai
import Data.Aeson (toJSON, Value, decode)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as C
import Control.Exception (bracket)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    businessLogicSpec

withUserApp :: IO() -> IO ()
withUserApp action =
    bracket (C.forkIO $ Warp.run 9999 appCourses)
      C.killThread
      (const action)

businessLogicSpec :: Spec
businessLogicSpec =
    around_ withUserApp $ do
        with (pure $ appCourses) $ do
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
matchAllUsers = matchBodyValue (toJSON users)
