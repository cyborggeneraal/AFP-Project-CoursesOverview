{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ApiCourses (appCourses, dummyCourses)
import Test.Hspec
import Test.Hspec.Wai
import Data.Aeson (toJSON, Value, decode)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Data.ByteString.Lazy.UTF8 (toString)

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
                  get "/courses" `shouldRespondWith` ResponseMatcher 200 [] (MatchBody matchDummyCourses)
            describe "GET /courses/course_id" $ do
                it "should get a course" $
                  get "/courses/101" `shouldRespondWith` 200
                it "should get an empty list" $ do
                  get "/courses/105" `shouldRespondWith` 200

matchDummyCourses :: [a] -> Body -> Maybe String 
matchDummyCourses _ body = case (decode body :: Maybe Value) of
    Just val | val == (toJSON dummyCourses) -> Nothing
    _ -> Just $ show (toJSON (toString body))
