{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiCourses where

import Servant
import Servant.Swagger

import Data.Swagger (Swagger)
import Data.List (find)
import Network.Wai.Middleware.Cors
import Data.List (isPrefixOf)
import Data.List (nub)
import Types

coursesAPI :: Proxy CoursesAPI
coursesAPI = Proxy

swaggerHandler :: Handler Swagger
swaggerHandler = return $ toSwagger coursesAPI

-- The Courses API is moved to the bottom of the file, so it could be displayed
-- alongside the server implementation and the handlers.

-- | Given a course ID, return the course with that ID, if it exists
-- Note that this method matches by the whole courseID, not by a prefix
getCourseByID :: String -> [Course] -> Maybe Course
getCourseByID targetID courses = find (\course -> courseID course == targetID) courses

courseExists :: String -> [Course] -> Bool
courseExists courseId courses = 
    let course = getCourseByID courseId courses
    in case course of
        Nothing             -> False
        Just _              -> True

getAllCoursesHandler :: Handler [Course]
getAllCoursesHandler = return dummyCourses

getCourseHandler :: String -> Handler [Course]
getCourseHandler _cid = return $ filter (\c -> isPrefixOf _cid (courseID c)) dummyCourses

getCoursesFromPrerequisites :: String -> [Prerequisite] -> [Course] -> [Course]
getCoursesFromPrerequisites targetCourseID prerequisites courses =
    [course | prereq <- prerequisites
                        , course <- courses
                        , pCourseID prereq == targetCourseID, courseID course == prerequisiteID prereq]

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

getUsersHandler :: Handler [User]
getUsersHandler = return users

getCoursesFromCTEntity :: String -> [CoursesTaken] -> [Course] -> [Course]
getCoursesFromCTEntity targetUser coursesTaken courses = 
    [course | courseTaken <- coursesTaken
            , course <- courses
            , userName courseTaken == targetUser
            , takenCourseID courseTaken == courseID course]

getUserCoursesHandler :: String -> Handler [Course]
getUserCoursesHandler nameUser = 
    return $ getCoursesFromCTEntity nameUser dummyCoursesTaken dummyCourses

getAllTracksHandler :: Handler [Track]
getAllTracksHandler = return dummyTracks

getTrackHandler :: String -> Handler [Track]
getTrackHandler _id = 
    return $ [track | track <- dummyTracks, trackID track == _id]

getCoursesTrack :: Track -> (String, [Course])
getCoursesTrack t = (trackName t, trackMand t ++ trackElec t)

getCoursesTracksHandler :: Handler [(String, [Course])]
getCoursesTracksHandler = return $ getCoursesTrack <$> dummyTracks

type CoursesAPI =
  "courses" :> Get '[JSON] [Course]                   
  :<|> "courses" :> Capture "courseID" String :> Get '[JSON] [Course]
  :<|> "courses" :> Capture "courseID" String :> "prereq" :> Get '[JSON] [Course]
  :<|> "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "name" String :> Get '[JSON] User
  :<|> "users" :> Capture "name" String :> "courses" :> Get '[JSON] [Course]
  :<|> "tracks" :> Get '[JSON] [Track]
  :<|> "tracks" :> Capture "trackID" String :> Get '[JSON] [Track]
  :<|> "tracks-courses" :> Get '[JSON] [(String, [Course])]

type SwaggerAPI = "API" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CoursesAPI

server :: Server API
server = swaggerHandler :<|>
            getAllCoursesHandler :<|>
            getCourseHandler  :<|>
            getPrereqsHandler :<|>
            return users :<|>
            getUserHandler :<|>
            getUserCoursesHandler :<|>
            getAllTracksHandler :<|>
            getTrackHandler :<|>
            getCoursesTracksHandler
            
api :: Proxy API
api = Proxy

appCourses :: Application
appCourses = simpleCors $ serve api server