{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiCourses where

import Servant
import Servant.Swagger

import Data.Swagger (Swagger)
import Data.List
import Network.Wai.Middleware.Cors
import Types
import Database

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

getUserHandler :: DatabasePath -> String -> Handler User
getUserHandler p nameUser = do
    users' <- allUsers p
    case [user | user <- users', name user == nameUser] of
        [] -> throwError err404
        (x:_) -> return x

getUsersHandler :: DatabasePath -> Handler [User]
getUsersHandler = allUsers

createUserHandler :: DatabasePath -> User -> Handler ()
createUserHandler p u = addDbUser p (userToDbUser u)

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
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] ()
  :<|> "tracks" :> Get '[JSON] [Track]
  :<|> "tracks" :> Capture "trackID" String :> Get '[JSON] [Track]
  :<|> "tracks-courses" :> Get '[JSON] [(String, [Course])]

type SwaggerAPI = "API" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> CoursesAPI

server :: DatabasePath -> Server API
server p = swaggerHandler :<|>
            getAllCoursesHandler :<|>
            getCourseHandler  :<|>
            getPrereqsHandler :<|>
            allUsers p :<|>
            getUserHandler p :<|>
            getUserCoursesHandler :<|>
            createUserHandler p :<|>
            getAllTracksHandler :<|>
            getTrackHandler :<|>
            getCoursesTracksHandler
            
api :: Proxy API
api = Proxy

appCourses :: DatabasePath -> Application
appCourses p = simpleCors $ serve api $ server p