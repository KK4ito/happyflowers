{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : HappyFlowers.Types
Description : Required types for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The types included in this module are used to represent data used throughout the
happy flowers project.
-}
module HappyFlowers.Types (
  -- * Types
  Event(..),
  Measurement(..),
  History(..),
  Settings(..)
  ) where

import Data.Aeson             (ToJSON)
import Database.SQLite.Simple (field, FromRow(..))
import GHC.Generics

-- | An event marks a watering, either `manual` or `automatic`. It can be parsed
-- from the database and serialised to JSON.
data Event =
  Event { eventId :: Int -- ^ The numeric ID of the event.
        , eventType :: String -- ^ The type of the event, either `automatic` or `manual`.
        , eventTimestamp :: String -- ^ The timestamp when the event occured.
        } deriving Generic

instance ToJSON Event
instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field

-- | A measurement marks a measurement of the moisture level of the flower. It
-- can be parsed from the database and serialised to JSON.
data Measurement =
  Measurement { measurementId :: Int -- ^ The numeric ID of the measurement.
              , measurementValue :: Int  -- ^ The moisture level measured at this point.
              , measurementTimestamp :: String -- ^ The timestamp when the measurement occured.
              } deriving Generic

instance ToJSON Measurement
instance FromRow Measurement where
  fromRow = Measurement <$> field <*> field <*> field

-- | History is a combination of events and measurement, forming the complete
-- historical data of the application over a given timeframe. It can be
-- serialised to JSON.
data History =
  History { events :: [Event] -- ^ The list of events that are part of the history.
          , measurements :: [Measurement] -- ^ The list of measurements that are part of the history.
          } deriving Generic

instance ToJSON History

-- | Settings is the entity containing all application data. It can be retrieved
-- from the database and serialised to JSON.
data Settings =
  Settings { name :: String -- ^ The name of the flower.
           , upper :: Int -- ^ The upper moisture level for the flower.
           , lower :: Int -- ^ The lower moisture level for the flower.
           , interval :: Int -- ^ The interval at which moisture levels should be measured.
           } deriving Generic

instance ToJSON Settings
instance FromRow Settings where
  fromRow = Settings <$> field <*> field <*> field <*> field
