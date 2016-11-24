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

module HappyFlowers.DB
  (
    -- * Configuration
    dbName
    -- * DB Operations
  , querySettings
  , updateSettings
  , queryHistory
  , addEvent
  , addMeasurement
  , queryLatestEvent
  , queryLatestMeasurement
  -- * Operations
  , getDate
  ) where

import           HappyFlowers.Config    (getConfig)
import           HappyFlowers.Type

import           Control.Exception      (try)
import           Data.Time.Clock        (getCurrentTime, utctDay)
import           Data.Time.Calendar     (addDays)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple as S

-- | 'dbName' determines which sqlite database is used for the application.
dbName :: String
dbName = "happyflowers.db"

-- | The 'querySettings' function attempts to retrieve settings data from the
-- database.
querySettings :: IO (Maybe Settings)
querySettings = do
  conn <- S.open dbName
  rows <- try (S.query_ conn "SELECT * FROM settings") :: IO (Either S.SQLError [Settings])
  S.close conn
  case rows of
    Right rows' -> return . Just $ head rows'
    Left _      -> return Nothing

-- | The 'updateSettings' function updates the settings entry in the database
-- with the passed data.
updateSettings :: S.ToRow a => a -- ^ New DB entry
               -> IO ()
updateSettings body = do
  conn <- S.open dbName
  S.execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" body
  S.close conn

-- | The 'getDate' function retrieves a sqlite-compatible timestamp based on the
-- offset that is passed as a parameter.
getDate :: Integer -- ^ Time frame in days
        -> IO String
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

  conn <- S.open dbName
  events <- try (S.query conn "SELECT * FROM events WHERE timestamp >= ? ORDER BY timestamp ASC" (S.Only timestamp)) :: IO (Either S.SQLError [Event])
  measurements <- try (S.query conn "SELECT * FROM measurements WHERE timestamp >= ? ORDER BY timestamp ASC" (S.Only timestamp)) :: IO (Either S.SQLError [Measurement])
  S.close conn

  case (events, measurements) of
    (Right e, Right m) -> return $ Just History { events = e
                                                , measurements = m
                                                }
    otherwise          -> return Nothing

-- TODO: document
addEvent :: String -- ^ Event kind
         -> IO ()
addEvent kind = do
  conn <- S.open dbName
  S.execute conn "INSERT INTO events (type) VALUES (?)" (S.Only (kind :: String))
  S.close conn

-- TODO: document
queryLatestEvent :: IO (Maybe Event)
queryLatestEvent = do
  conn <- S.open dbName
  rows <- try (S.query_ conn "SELECT * FROM events ORDER BY timestamp DESC LIMIT 1") :: IO (Either S.SQLError [Event])
  S.close conn
  case rows of
    Right rows' -> return . Just $ head rows'
    Left _      -> return Nothing

-- TODO: document
addMeasurement :: Int -- ^ Measurement value
               -> IO ()
addMeasurement value = do
  conn <- S.open dbName
  S.execute conn "INSERT INTO measurements (value) VALUES (?)" (S.Only (value :: Int))
  S.close conn

-- TODO: document
queryLatestMeasurement :: IO (Maybe Measurement)
queryLatestMeasurement = do
  conn <- S.open dbName
  rows <- try (S.query_ conn "SELECT * FROM measurements ORDER BY timestamp DESC LIMIT 1") :: IO (Either S.SQLError [Measurement])
  S.close conn
  case rows of
    Right rows' -> return . Just $ head rows'
    Left _      -> return Nothing
