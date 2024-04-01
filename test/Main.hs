{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ApiCourses (appCourses)
import Types (dummyCourses)
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
                  get "/courses" `shouldRespondWith` 200 {matchBody = MatchBody matchDummyCourses}
            describe "GET /courses/course_id" $ do
                it "should get a course" $
                  get "/courses/101" `shouldRespondWith` 200 {matchBody = MatchBody matchCourse101}
                it "should get an empty list" $
                  get "/courses/105" `shouldRespondWith` 200 {matchBody = MatchBody matchEmptyList}
            describe "GET /courses/course_id/prereq" $ do
                it "should return 101 for 201" $
                  get "/courses/201/prereq"  `shouldRespondWith` 200 {matchBody = MatchBody matchCourse101}
                it "should return empty for 101" $
                  get "/courses/101/prereq" `shouldRespondWith` 200 {matchBody = MatchBody matchEmptyList}
                it "should return empty for 105" $
                  get "/courses/105/prereq" `shouldRespondWith` 200 {matchBody = MatchBody matchEmptyList}

matchDummyCourses :: [a] -> Body -> Maybe String 
matchDummyCourses _ body = case (decode body :: Maybe Value) of
    Just val | val == (toJSON dummyCourses) -> Nothing
    _ -> Just $ "Error"

matchCourse101:: [a] -> Body -> Maybe String
matchCourse101 _ body = case (decode body :: Maybe Value) of
    Just val | val == (toJSON (take 1 dummyCourses)) -> Nothing
    _ -> Just "Error"

matchEmptyList :: [a] -> Body -> Maybe String
matchEmptyList _ body = case (decode body :: Maybe Value) of
    Just val | val == (toJSON ([] :: [String])) -> Nothing
    _ -> Just "Error"
