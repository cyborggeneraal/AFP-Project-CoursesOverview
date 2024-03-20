{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.Set as Set (Set, fromList) 

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
type TimeSlot = Set.Set TS

instance ToJSON Level
instance ToJSON Course
instance ToJSON Term
instance ToJSON TS

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

users :: [User]
users =
    [ User "IsaacNewton"     372 "isaac@newton.co.uk" (fromGregorian 1683 3 1) 
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]


dummyCourses :: [Course]
dummyCourses =
    [ Course { term = S1
             , timeSlot = Set.fromList [A, B]
             , courseID = "101"
             , level = Bachelor
             , ecName = "Functional Programming"
             , capacity = 30
             }
    , Course { term = S2
             , timeSlot = Set.fromList [A, B]
             , courseID = "201"
             , level = Master
             , ecName = "Advanced Functional Programming"
             , capacity = 25
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