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
  -- * Operations
  querySettings,
  updateSettings,
  queryHistory
  ) where

import Control.Exception
import Database.SQLite.Simple
import HappyFlowers.Types

-- | 'dbName' determines which sqlite database is used for the application.
dbName :: String
dbName = "happyflowers.db"

-- | The 'querySettings' function attempts to retrieve settings data from the
-- database.
querySettings :: IO (Either SQLError [Settings])
querySettings = do
  conn <- open dbName
  rows <- try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings])
  close conn
  return rows

-- | The 'updateSettings' function updates the settings entry in the database
-- with the passed data.
updateSettings :: ToRow a => a -> IO ()
updateSettings body = do
  conn <- open dbName
  execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" body
  close conn

-- | The 'queryHistory' function attempts to retrieve historical data. This
-- includes recent measurements and events.
queryHistory :: IO (Either SQLError History)
queryHistory = do
  conn <- open dbName
  events <- try (query_ conn "SELECT * FROM events WHERE date(timestamp) >= date('now', '14days') ORDER BY timestamp ASC") :: IO (Either SQLError [Event])
  measurements <- try (query_ conn "SELECT * FROM measurements WHERE date(timestamp) >= date('now', '-14days') ORDER BY timestamp ASC") :: IO (Either SQLError [Measurement])
  close conn
  case (events, measurements) of
    (Right e, Right m) -> return $ Right History { events = e
                                                 , measurements = m
                                                 }
    (Left e, Right _)  -> return $ Left e
    (Right _, Left e)  -> return $ Left e
    (Left e1, Left e2) -> return $ Left e1
