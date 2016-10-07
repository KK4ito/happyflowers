{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (ToJSON)
import GHC.Generics
import Network.HTTP.Types.Status (notImplemented501)
import Web.Scotty

data Settings = Settings {
  name :: String,
  upper :: Int,
  lower :: Int,
  interval :: Int
} deriving (Show, Generic)

instance ToJSON Settings

data Event = Event {
  eventType :: String,
  eventTimestamp :: Int
} deriving (Show, Generic)

instance ToJSON Event

data Measurement = Measurement {
  measurementValue :: Int,
  measurementTimestamp :: Int
} deriving (Show, Generic)

instance ToJSON Measurement

data History = Hisotry {
  events :: [Event],
  measurements :: [Measurement]
} deriving (Show, Generic)

instance ToJSON History

main = scotty 3000 $ do
  get "/settings" $ do
    status notImplemented501
  put "/settings" $ do
    status notImplemented501
  get "/history" $ do
    status notImplemented501
