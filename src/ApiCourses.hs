{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ApiCourses where

import Servant
import Servant.Swagger
import GHC.Generics

import Data.Aeson
import Data.Swagger
import Data.List (find)
import Data.ByteString.Lazy.Char8 (pack)

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

getCourseByID :: Int -> [Course] -> Maybe Course
getCourseByID targetID courses = find (\course -> courseID course == targetID) courses

courseExists :: Int -> [Course] -> Bool
courseExists courseId courses = 
    let course = getCourseByID courseId courses
    in case course of
        Nothing             -> False
        Just _              -> True

getCoursesHandler :: Handler [Course]
getCoursesHandler = return dummyCourses

getCourseHandler :: Int -> Handler Course
getCourseHandler courseId = do
    maybeCourse <- return $ getCourseByID courseId dummyCourses
    case maybeCourse of
        Nothing                 -> throwError courseNotFoundError
        Just course             -> return course

    where courseNotFoundError   = err404 { errBody = pack $ "No course with ID " ++ show courseId ++ " was found!" }

data Prerequisite = Prerequisite
    {
        pCourseID :: Int,
        prerequisiteID :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Prerequisite

dummyPrerequisites :: [Prerequisite]
dummyPrerequisites = 
        [ Prerequisite { 
            pCourseID = 201,
            prerequisiteID = 101
            }
        ]

getPrereqs :: Int -> [Prerequisite] -> Maybe [Prerequisite]
getPrereqs targetCourseID prerequisites =
    let prereqs = filter (\prereq -> pCourseID prereq == targetCourseID) prerequisites
    in case prereqs of
        []                      -> Nothing
        _                       -> Just prereqs

getCoursesFromPrerequisites :: Int -> [Prerequisite] -> [Course] -> Maybe [Course]
getCoursesFromPrerequisites targetCourseID prerequisites courses =
    let prereqs = getPrereqs targetCourseID prerequisites
    in case prereqs of
        Nothing                 -> Nothing
        Just filteredPrereqs    -> Just [course | prereq <- filteredPrereqs,
                                                course <- courses,
                                                courseID course == prerequisiteID prereq]

getPrereqsHandler :: Int -> Handler [Course]
getPrereqsHandler courseId = 
    if courseExists courseId dummyCourses then
        do
            maybeCourses <- return $ getCoursesFromPrerequisites courseId dummyPrerequisites dummyCourses
            case maybeCourses of
                Nothing             -> throwError prerequisiteNotFoundError
                Just courses        -> return courses
    else 
        throwError courseNotFoundError
    where 
        courseNotFoundError         = err404 { errBody = pack $ "No course with ID " ++ show courseId ++ " was found!" }
        prerequisiteNotFoundError   = err503 { errBody = pack $ "No prerequisites exist for course with ID: " ++ show courseId }

server :: Server API
server = swaggerHandler :<|> getCoursesHandler :<|> getCourseHandler :<|> getPrereqsHandler

coursesApi :: Proxy API
coursesApi = Proxy

appCourses :: Application
appCourses = simpleCors $ serve coursesApi server