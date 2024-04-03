module Main where
import ApiCourses(appCourses)
import Network.Wai.Handler.Warp
import System.Directory

import Database
import Database.SQLite.Simple
import Control.Monad

main :: IO ()
main = do
  exists <- doesFileExist "database.db"
  conn <- open "database.db"
  
  when exists $ createDatabase conn
  run 8081 $ appCourses "database.db"