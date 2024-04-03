{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Types where

import Data.Swagger (ToSchema)
import GHC.Generics
import Data.Aeson
import Data.Time
import Data.Set as Set (Set, fromList, toList) 

data Course = Course
    {
        courseID :: CourseID
    ,   term :: Term
    ,   timeSlot :: TimeSlot
    ,   level :: Level
    ,   ecName :: String
    ,   capacity :: Int
    } deriving (Eq, Show, Generic)

data Level = Bachelor | Master | PhD
    deriving (Eq, Show, Generic)

data Term = B1 | B2 | B3 | B4 | S1 | S2
    deriving (Eq, Show, Generic)

data TS = A | B | C | D
    deriving (Eq, Show, Generic, Ord)

type CourseID = String
data TimeSlot = TS (Set.Set TS)
    deriving (Eq, Generic)

instance Show TimeSlot where
    show (TS ts) = foldr (\x acc -> show x ++ acc) "" (Set.toList ts)

instance ToJSON Level
instance ToJSON Course
instance ToJSON Term
instance ToJSON TS
instance ToJSON TimeSlot where
    toJSON (TS ts) = toJSON $ foldr (\x acc -> show x ++ acc) "" (Set.toList ts)

instance ToSchema Term
instance ToSchema Level
instance ToSchema TS
instance ToSchema TimeSlot

instance ToSchema Course

data Prerequisite = Prerequisite
    {
        pCourseID :: CourseID,
        prerequisiteID :: CourseID
    } deriving (Eq, Show, Generic)

data CoursesTaken = CoursesTaken
    {
        userName :: String,
        takenCourseID :: CourseID
    } deriving (Eq, Show, Generic)

instance ToJSON CoursesTaken

data SortBy = Age | Name

data User = User 
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)

instance ToJSON User

instance ToSchema User

data Track = Track
    {
        trackID :: String,
        trackName :: String,
        trackMand :: [Course],
        trackElec :: [Course]
    } deriving (Show, Generic)

users :: [User]
users =
    [ User "IsaacNewton"     372 "isaac@newton.co.uk" (fromGregorian 1683 3 1) 
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

-- data Track = Track 
--     { trackName :: String
--     , trackID :: String
--     } deriving (Eq, Show, Generic)

instance ToJSON Track

data CourseTrack = CourseTrack
    { cID :: String
    , tID :: String
    } deriving (Eq, Show, Generic)

instance ToJSON CourseTrack
instance ToSchema Track

dummyCourses :: [Course]
dummyCourses =
    [ Course { term = S1
             , timeSlot = TS $ Set.fromList [A, B]
             , courseID = "101"
             , level = Bachelor
             , ecName = "Functional Programming"
             , capacity = 30
             }
    , Course { term = S2
             , timeSlot = TS $ Set.fromList [A, B]
             , courseID = "201"
             , level = Master
             , ecName = "Advanced Functional Programming"
             , capacity = 25
             }
    , Course { term = S2
             , timeSlot = TS $ Set.fromList [A]
             , courseID = "202"
             , level = Master
             , ecName = "Advanced Algorithms"
             , capacity = 20
             }
    ]

dummyPrerequisites :: [Prerequisite]
dummyPrerequisites = 
        [ Prerequisite { 
            pCourseID = "201",
            prerequisiteID = "101"
            }
        ]

dummyCoursesTaken :: [CoursesTaken]
dummyCoursesTaken = 
    [ CoursesTaken {
        userName = "IsaacNewton",
        takenCourseID = "101"
    }
    ]

dummyTracks :: [Track]
dummyTracks = 
    [Track {
        trackID = "1",
        trackName = "Programming Technology",
        trackMand = filter ((== "101") . courseID) dummyCourses,
        trackElec = filter ((== "201") . courseID) dummyCourses
    }]

