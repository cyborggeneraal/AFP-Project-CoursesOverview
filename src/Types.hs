{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Time

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

data CoursesTaken = CoursesTaken
    {
        userName :: String,
        takenCourseID :: String
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