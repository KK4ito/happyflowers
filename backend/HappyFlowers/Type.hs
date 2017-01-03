{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : HappyFlowers.Type
Description : Required types for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Type
    (
      -- * Types
      Event(..)
    , Measurement(..)
    , History(..)
    , Settings(..)
    , BusyState(..)
    , WSEventKind(..)
    , MeasurementKind(..)
    , EventKind(..)
    , Command(..)
    , Client(..)
    , ServerState(..)
    ) where

import Data.Aeson             (ToJSON)
import Data.Text              (Text)
import Database.SQLite.Simple (field, FromRow(..))
import GHC.Generics
import Network.WebSockets     (Connection)

-- | An event marks a watering, either `manual` or `automatic`.
data Event = Event
    { eventId :: !Int         -- ^ Numeric ID
    , eventKind :: !Text      -- ^ Event kind, `automatic` or `manual`
    , eventTimestamp :: !Text -- ^ Event timestamp
    } deriving Generic

instance ToJSON Event
instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field

-- | A measurement marks the checking of the flower's moisture level or its
-- temperature.
data Measurement = Measurement
    { measurementId :: !Int         -- ^ Numeric ID
    , measurementKind :: !Text      -- ^ Measurement kind
    , measurementValue :: !Int      -- ^ Moisture level / temperature
    , measurementTimestamp :: !Text -- ^ Measurement timestamp
    } deriving Generic

instance ToJSON Measurement
instance FromRow Measurement where
    fromRow = Measurement <$> field <*> field <*> field <*> field

-- | History is a combination of events and measurement, forming the complete
-- historical data of the application.
data History = History
    { events :: ![Event]             -- ^ Historical events data
    , measurements :: ![Measurement] -- ^ Historical measurement data
    } deriving Generic

instance ToJSON History

-- | Settings contains all application settings data.
data Settings = Settings
    { name :: !Text    -- ^ Flower name
    , upper :: !Int    -- ^ Upper moisture limit
    , lower :: !Int    -- ^ Lower moisture limit
    , interval :: !Int -- ^ Measurement interval
    } deriving Generic

instance ToJSON Settings
instance FromRow Settings where
    fromRow = Settings <$> field <*> field <*> field <*> field

-- | A flower is busy if it is currently communicating with the sensor or the
-- pump. No overlapping actions may happen.
data BusyState
    = Busy
    | Idle
    deriving Eq

-- | A WebSockets event has a kind property that determines the type of the
-- event.
data WSEventKind
    = MeasurementReceived
    | EventReceived
    | HistoryReceived
    | SettingsChanged
    | BusyChanged
    | TriggerPump
    deriving Show

-- | The sensor measures both the moisture of the flower and its temperature.
data MeasurementKind
    = Moisture
    | Temperature
    deriving Show

-- | An event is either automatic (produced by the interval measurement) or
-- manual (triggered by the user).
data EventKind
    = Automatic
    | Manual
    deriving Show

-- | A command is used to control the hardware processes and the involved tasks.
data Command
    = SaveEvent EventKind
    | SaveMeasurement MeasurementKind Int
    | UpdateBusy BusyState
    | PumpRequired Int
    | CheckRequired

-- | 'Client' is a type alias that connects a unique ID with a WebSockets
-- connection.
type Client = (Text, Connection)

-- | keeps track of the currently available Id and the list of connected
-- clients.
type ServerState = [Client]
