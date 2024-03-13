module Main where

import ApiCourses(appCourses)
import Network.Wai.Handler.Warp

main :: IO ()
main = do

  run 8081 $ appCourses