{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiCourses where

import Servant
import Data.List (find)
import GHC.Generics
import Data.Aeson
import Network.Wai.Middleware.Cors
import Data.List (isPrefixOf)


data Course = Course
    {
        term :: Int
    ,   timeSlot :: String
    ,   courseID :: String
    ,   level :: String
    ,   ecName :: String
    ,   capacity :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Course

data Prerequisite = Prerequisite
    {
        pCourseID :: String,
        prerequisiteID :: String
    } deriving (Eq, Show, Generic)

type CoursesAPI =
  "courses" :> Get '[JSON] [Course]                   
  :<|> "courses" :> Capture "courseID" String :> Get '[JSON] [Course]
  :<|> "courses" :> Capture "courseID" String :> "prereq" :> Get '[JSON] [Course]

dummyCourses :: [Course]
dummyCourses =
    [ Course { term = 1
             , timeSlot = "AB"
             , courseID = "101"
             , level = "Bachelor"
             , ecName = "Functional Programming"
             , capacity = 30
             }
    , Course { term = 2
             , timeSlot = "C"
             , courseID = "201"
             , level = "Master"
             , ecName = "Advanced Functional Programming"
             , capacity = 25
             }
    ]

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

dummyPrerequisites :: [Prerequisite]
dummyPrerequisites = 
        [ Prerequisite { 
            pCourseID = "201",
            prerequisiteID = "101"
            }
        ]

getPrereqs :: String -> [Prerequisite] -> Maybe [Prerequisite]
getPrereqs targetCourseID prerequisites =
    let prereqs = filter (\prereq -> pCourseID prereq == targetCourseID) prerequisites
    in case prereqs of
        []                      -> Nothing
        _                       -> Just prereqs

getCoursesFromPrerequisites :: String -> [Prerequisite] -> [Course] -> [Course]
getCoursesFromPrerequisites targetCourseID prerequisites courses =
    [course | prereq <- prerequisites, course <- courses, pCourseID prereq == targetCourseID, courseID course == prerequisiteID prereq]

getPrereqsHandler :: String -> Handler [Course]
getPrereqsHandler courseId = 
    if courseExists courseId dummyCourses then
        return $ getCoursesFromPrerequisites courseId dummyPrerequisites dummyCourses
    else 
        return []

server :: Server CoursesAPI
server = getAllCoursesHandler :<|>
            getCourseHandler  :<|>
            getPrereqsHandler

coursesApi :: Proxy CoursesAPI
coursesApi = Proxy

appCourses :: Application
appCourses = simpleCors $ serve coursesApi server