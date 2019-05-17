{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module LevelUp where

import Control.Lens
import Data.Monoid
import Data.Text

-- examples taken from https://williamyaoh.com/posts/2019-04-25-lens-exercises.html

data User = User
  { _name     :: Text
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [Text]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo

user1 = User
  { _name = "christian.henry"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

-- same as: view (metadata . numLogins) user1
-- or if you want it to (kind of) look like java: user1^.metadata.numLogins
getNumLogins :: Int
getNumLogins = user1 ^. metadata . numLogins

updateMultipleFields :: User
updateMultipleFields = user1
  & userid .~ 104
  & metadata . numLogins %~ (+1)
  & metadata . associatedIPs %~ (:) "127.0.0.1"

users = 
  [ User
    { _name = "christian.henry"
    , _userid = 101
    , _metadata = UserInfo
      { _numLogins = 0
      , _associatedIPs =
        [ "52.39.193.61"
        , "52.39.193.75"
        ]
      }
    }
  , User
    { _name = "user2"
    , _userid = 102
    , _metadata = UserInfo
      { _numLogins = 5
      , _associatedIPs = []
      }
    }
  , User
    { _name = "user3"
    , _userid = 103
    , _metadata = UserInfo
      { _numLogins = 100
      , _associatedIPs =
        [ "52.39.193.61"
        , "52.39.193.75"
        ]
      }
    }
  ]

-- can think of "traversed" as a lens to every value in a list
prefixNames = users 
  & traversed . name %~ (\x -> "test." <> x)

-- foldMapOf takes a lens (where to point), a function to apply to everything, and then accumulates them
sumLogins = getSum $ users
  & foldMapOf (traversed . metadata . numLogins) Sum

getAllIPs1 = users 
  & foldMapOf (traversed . metadata . associatedIPs) id

-- point to all users, the metadata / associatedIP for each, then to each actual IP value
getAllIPs2 = users ^.. traversed . metadata . associatedIPs . traversed