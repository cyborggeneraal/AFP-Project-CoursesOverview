{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiCourses where

import Servant
import Servant.Swagger
import GHC.Generics

import Data.Aeson
import Data.Swagger
import Data.List (find)
import Network.Wai.Middleware.Cors
import Data.List (isPrefixOf)
import Types

import Network.Wai.Middleware.Cors

coursesAPI :: Proxy CoursesAPI
coursesAPI = Proxy

type CoursesAPI =
  "courses" :> Get '[JSON] [Course]                   
  :<|> "courses" :> Capture "courseID" Int :> Get '[JSON] Course
  :<|> "courses" :> Capture "courseID" Int :> "prereq" :> Get '[JSON] [Course]

type SwaggerAPI = "API" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CoursesAPI

data Course = Course
    {
        term :: Int
    ,   timeSlot :: String
    ,   courseID :: Int
    ,   level :: String
    ,   ecName :: String
    ,   capacity :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Course

instance ToSchema Course

swaggerHandler :: Handler Swagger
swaggerHandler = return $ toSwagger coursesAPI

dummyCourses :: [Course]
dummyCourses =
    [ Course { term = 1
             , timeSlot = "AB"
             , courseID = 101
             , level = "Bachelor"
             , ecName = "Functional Programming"
             , capacity = 30
             }
    , Course { term = 2
             , timeSlot = "C"
             , courseID = 201
             , level = "Master"
             , ecName = "Advanced Functional Programming"
             , capacity = 25
             }
    ]
-- The Courses API is moved to the bottom of the file, so it could be displayed
-- alongside the server implementation and the handlers.

-- | Given a course ID, return the course with that ID, if it exists
-- Note that this method matches by the whole courseID, not by a prefix
getCourseByID :: String -> [Course] -> Maybe Course
getCourseByID targetID courses = find (\course -> courseID course == targetID) courses

-- | Given a course ID we search if it is a prefix of any courseID in our set
-- This method is used in the main search, the one that uses event listeners.
getCourseByPrefixID :: String -> [Course] -> Maybe Course
getCourseByPrefixID targetID courses = find (\course -> targetID `isPrefixOf` courseID course) courses

courseExists :: String -> [Course] -> Bool
courseExists courseId courses = 
    let course = getCourseByID courseId courses
    in case course of
        Nothing             -> False
        Just _              -> True

getAllCoursesHandler :: Handler [Course]
getAllCoursesHandler = return dummyCourses

getCourseHandler :: String -> Handler [Course]
getCourseHandler courseId = 
    case getCourseByPrefixID courseId dummyCourses of
        Nothing                 -> return []
        Just course             -> return [course]

getCoursesFromPrerequisites :: String -> [Prerequisite] -> [Course] -> [Course]
getCoursesFromPrerequisites targetCourseID prerequisites courses =
    [course | prereq <- prerequisites, course <- courses, pCourseID prereq == targetCourseID, courseID course == prerequisiteID prereq]

getPrereqsHandler :: String -> Handler [Course]
getPrereqsHandler courseId = 
    if courseExists courseId dummyCourses then
        return $ getCoursesFromPrerequisites courseId dummyPrerequisites dummyCourses
    else 
        return []

getUserHandler :: String -> Handler User
getUserHandler nameUser = 
    case [user | user <- users, name user == nameUser] of
        [] -> throwError err404
        (x:_) -> return x

getCoursesFromCTEntity :: String -> [CoursesTaken] -> [Course] -> [Course]
getCoursesFromCTEntity targetUser coursesTaken courses = 
    [course | courseTaken <- coursesTaken, course <- courses, userName courseTaken == targetUser, takenCourseID courseTaken == courseID course]

server :: Server API
server = swaggerHandler :<|> getCoursesHandler :<|> getCourseHandler :<|> getPrereqsHandler

coursesApi :: Proxy API
getUserCoursesHandler :: String -> Handler [Course]
getUserCoursesHandler nameUser = 
    return $ getCoursesFromCTEntity nameUser dummyCoursesTaken dummyCourses

type CoursesAPI =
  "courses" :> Get '[JSON] [Course]                   
  :<|> "courses" :> Capture "courseID" String :> Get '[JSON] [Course]
  :<|> "courses" :> Capture "courseID" String :> "prereq" :> Get '[JSON] [Course]
  :<|> "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "name" String :> Get '[JSON] User
  :<|> "users" :> Capture "name" String :> "courses" :> Get '[JSON] [Course]

server :: Server CoursesAPI
server = getAllCoursesHandler :<|>
            getCourseHandler  :<|>
            getPrereqsHandler :<|>
            return users :<|>
            getUserHandler :<|>
            getUserCoursesHandler

coursesApi :: Proxy CoursesAPI
coursesApi = Proxy

appCourses :: Application
appCourses = simpleCors $ serve coursesApi server