{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.DB
Description : Database access for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to query and store data in an
sqlite database.
-}

module HappyFlowers.DB (
  -- * Configuration
  dbName,
  -- * DB Operations
  querySettings,
  updateSettings,
  queryHistory,
  addEvent,
  addMeasurement,
  queryLatestEvent,
  queryLatestMeasurement,
  -- * Operations
  getDate
  ) where

import Control.Exception
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Database.SQLite.Simple
import HappyFlowers.Config
import HappyFlowers.Types

-- | 'dbName' determines which sqlite database is used for the application.
dbName :: String
dbName = "happyflowers.db"

-- | The 'querySettings' function attempts to retrieve settings data from the
-- database.
querySettings :: IO (Maybe Settings)
querySettings = do
  conn <- open dbName
  rows <- try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings])
  close conn
  case rows of
    Right rows' -> return $ Just $ head rows'
    Left _      -> return Nothing

-- | The 'updateSettings' function updates the settings entry in the database
-- with the passed data.
updateSettings :: ToRow a => a -> IO ()
updateSettings body = do
  conn <- open dbName
  execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" body
  close conn

-- | The 'getDate' function retrieves a sqlite-compatible timestamp based on the
-- offset that is passed as a parameter.
getDate :: Integer -> IO String
getDate ago = do
  time <- getCurrentTime
  let refDate = addDays (ago * (-1)) $ utctDay time
  return $ formatTime defaultTimeLocale "%F" refDate

-- | The 'queryHistory' function attempts to retrieve historical data. This
-- includes recent measurements and events.
queryHistory :: IO (Maybe History)
queryHistory = do
  frame <- getConfig "frame"
  timestamp <- getDate (read frame :: Integer)

  conn <- open dbName
  events <- try (query conn "SELECT * FROM events WHERE timestamp >= ? ORDER BY timestamp ASC" (Only timestamp)) :: IO (Either SQLError [Event])
  measurements <- try (query conn "SELECT * FROM measurements WHERE timestamp >= ? ORDER BY timestamp ASC" (Only timestamp)) :: IO (Either SQLError [Measurement])
  close conn

  case (events, measurements) of
    (Right e, Right m) -> return $ Just History { events = e
                                                , measurements = m
                                                }
    otherwise          -> return Nothing

-- TODO: document
addEvent :: String -> IO ()
addEvent kind = do
  conn <- open dbName
  execute conn "INSERT INTO events (type) VALUES (?)" (Only (kind :: String))
  close conn

-- TODO: document
queryLatestEvent :: IO (Maybe Event)
queryLatestEvent = do
  conn <- open dbName
  rows <- try (query_ conn "SELECT * FROM events ORDER BY timestamp DESC LIMIT 1") :: IO (Either SQLError [Event])
  close conn
  case rows of
    Right rows' -> return $ Just $ head rows'
    Left _      -> return Nothing

-- TODO: document
addMeasurement :: Int -> IO ()
addMeasurement value = do
  conn <- open dbName
  execute conn "INSERT INTO measurements (value) VALUES (?)" (Only (value :: Int))
  close conn

-- TODO: document
queryLatestMeasurement :: IO (Maybe Measurement)
queryLatestMeasurement = do
  conn <- open dbName
  rows <- try (query_ conn "SELECT * FROM measurements ORDER BY timestamp DESC LIMIT 1") :: IO (Either SQLError [Measurement])
  close conn
  case rows of
    Right rows' -> return $ Just $ head rows'
    Left _      -> return Nothing
