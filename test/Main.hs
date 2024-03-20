{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ApiCourses (appCourses, dummyCourses)
import Test.Hspec
import Test.Hspec.Wai
import Data.Aeson (toJSON)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Data.ByteString.Lazy.UTF8

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
                  get "/courses" `shouldRespondWith` 200
            describe "GET /courses/course_id" $ do
                it "should get a course" $
                  get "/courses/101" `shouldRespondWith` 200
                it "should get an empty list" $ do
                  get "/courses/105" `shouldRespondWith` 200

matchTest :: [a] -> Body -> Maybe String 
matchTest _ body
    | toString body == show (toJSON dummyCourses) = Nothing
    | otherwise = Just "Error"
