{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time
import GHC.Generics
import Servant
import Data.Aeson
import Network.Wai

type UserAPI = "users" :> Get '[JSON] [User]

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

server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server