{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ApiCourses (appCourses, dummyCourses)
import Test.Hspec
import Test.Hspec.Wai
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
                  get "/courses" `shouldRespondWith` dummyCourses 
    
    -- withUserApp $ do
    --     let createUser = client (Proxy :: Proxy CoursesAPI)
    --     baseUrl <- runIO $ parseBaseUrl "http://localhost"
    --     manager <- runIO $ newManager defaultManagerSettings
    --     let clientEnv port = mkClientEnv manager undefined

    --     describe "GET /courses" $ do
    --         it "should get courses" $ \port -> do
    --             result <- runClientM getAllCoursesHandler (clientEnv port)
    --             result `shouldBe` (Right dummyCourses)
