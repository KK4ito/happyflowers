{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : HappyFlowers.Type
Description : Required types for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The types included in this module are used to represent data used throughout the
happy flowers project.
-}
module HappyFlowers.Type
    (
      -- * Types
      Event(..)
    , Measurement(..)
    , History(..)
    , Settings(..)
    ) where

import Data.Aeson             (ToJSON)
import Data.Text              (Text)
import Database.SQLite.Simple (field, FromRow(..))
import GHC.Generics

-- | An event marks a watering, either `manual` or `automatic`.
data Event = Event
    { eventId :: !Int           -- ^ Numeric ID
    , eventType :: !Text        -- ^ Event type, `automatic` or `manual`
    , eventTimestamp :: !Text   -- ^ Event timestamp
    } deriving Generic

instance ToJSON Event
instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field

-- | A measurement marks the checking of the flower's moisture level.
data Measurement = Measurement
    { measurementId :: !Int           -- ^ Numeric ID
    , measurementValue :: !Int        -- ^ Moisture level
    , measurementTimestamp :: !Text   -- ^ Measurement timestamp
    } deriving Generic

instance ToJSON Measurement
instance FromRow Measurement where
    fromRow = Measurement <$> field <*> field <*> field

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
