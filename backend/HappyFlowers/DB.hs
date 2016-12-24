{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.DB
Description : Database access for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.DB
    (
      -- * Settings
      querySettings
    , updateSettings
      -- * History
    , queryHistory
    , addEvent
    , addMeasurement
    , queryLatestEvent
    , queryLatestMeasurement
    ) where

import           Control.Exception      (try)
import           Data.Text              (Text, unpack)
import           Data.Time.Clock        (getCurrentTime, utctDay, UTCTime(..))
import           Data.Time.Calendar     (addDays)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple as S

import           HappyFlowers.Config    (getConfig)
import           HappyFlowers.Type

-- | determines which sqlite database is used for the application.
dbName :: String
dbName = "happyflowers.db"

-- | gets a single entry from a list database rows.
getSingleEntry :: S.FromRow a => Either S.SQLError [a] -> Maybe a
getSingleEntry = either (const Nothing) (\r -> Just $ head r)

-- | retrieves settings. Returns Nothing if the query fails.
querySettings :: IO (Maybe Settings)
querySettings = do
    conn <- S.open dbName
    rows <- try (S.query_ conn "SELECT * FROM settings") :: IO (Either S.SQLError [Settings])
    S.close conn
    return $ getSingleEntry rows

-- | updates settings using a record containing new data.
updateSettings :: S.ToRow a
               => a -- New DB entry
               -> IO ()
updateSettings body = do
    conn <- S.open dbName
    S.execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" body
    S.close conn

-- | retrieves historical data including events, measurements, and temperatures.
-- Returns Nothing if any of the queries fail.
queryHistory :: IO (Maybe History)
queryHistory = do
    frame <- getConfig "frame"
    current <- getCurrentTime
    let timestamp = getReferenceDate current (read (unpack frame) :: Integer)

    conn <- S.open dbName
    events <- try (S.query conn "SELECT * FROM events WHERE timestamp >= ? ORDER BY timestamp ASC" (S.Only (timestamp :: String))) :: IO (Either S.SQLError [Event])
    measurements <- try (S.query conn "SELECT * FROM measurements WHERE timestamp >= ? ORDER BY timestamp ASC" (S.Only (timestamp :: String))) :: IO (Either S.SQLError [Measurement])
    S.close conn

    case (events, measurements) of
        (Right e, Right m) -> return $ Just History { events = e
                                                    , measurements = m
                                                    }
        otherwise          -> return Nothing

-- | adds a new event entity of the given kind to the database.
addEvent :: EventKind -> IO ()
addEvent kind = do
    conn <- S.open dbName
    S.execute conn "INSERT INTO events (kind) VALUES (?)" (S.Only ((show kind) :: String))
    S.close conn

-- | retrieves the latest event entity from the database.
queryLatestEvent :: IO (Maybe Event)
queryLatestEvent = do
    conn <- S.open dbName
    rows <- try (S.query_ conn "SELECT * FROM events ORDER BY timestamp DESC LIMIT 1") :: IO (Either S.SQLError [Event])
    S.close conn
    return $ getSingleEntry rows

-- | adds a new measurement entity with the given value to the database.
addMeasurement :: MeasurementKind -> Int -> IO ()
addMeasurement kind value = do
    conn <- S.open dbName
    S.execute conn "INSERT INTO measurements (kind, value) VALUES (?, ?)" ((show kind) :: String, value :: Int)
    S.close conn

-- | retrieves the latest measurement entity from the database.
queryLatestMeasurement :: MeasurementKind -> IO (Maybe Measurement)
queryLatestMeasurement kind = do
    conn <- S.open dbName
    rows <- try (S.query conn "SELECT * FROM measurements WHERE kind = ? ORDER BY timestamp DESC LIMIT 1" (S.Only ((show kind) :: String))) :: IO (Either S.SQLError [Measurement])
    S.close conn
    return $ getSingleEntry rows

-- | retrieve a sqlite-compatible timestamp based on a given offset.
getReferenceDate :: UTCTime -- ^ Current time
                 -> Integer -- ^ Offset in days
                 -> String
getReferenceDate time ago = do
    let date = addDays (ago * (-1)) $ utctDay time
    formatTime defaultTimeLocale "%F" date
