{-# LANGUAGE DeriveGeneric #-}

module REST.Model (
  Event(..),
  Measurement(..),
  History(..),
  Settings(..)
) where

import Data.Aeson (ToJSON)
import Database.SQLite.Simple.FromRow
import GHC.Generics

data Event = Event {
  eventId :: Int,
  eventType :: String,
  eventTimestamp :: String
} deriving (Show, Generic)

instance ToJSON Event
instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field

data Measurement = Measurement {
  measurementId :: Int,
  measurementValue :: Int,
  measurementTimestamp :: String
} deriving (Show, Generic)

instance ToJSON Measurement
instance FromRow Measurement where
  fromRow = Measurement <$> field <*> field <*> field

data History = History {
  events :: [Event],
  measurements :: [Measurement]
} deriving (Show, Generic)

instance ToJSON History

data Settings = Settings {
  name :: String,
  upper :: Int,
  lower :: Int,
  interval :: Int
} deriving (Show, Generic)

instance ToJSON Settings
instance FromRow Settings where
  fromRow = Settings <$> field <*> field <*> field <*> field
