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

-- todo: document
dbName :: String
dbName = "happyflowers.db"

-- todo: document
querySettings :: IO (Either SQLError [Settings])
querySettings = do
  conn <- open dbName
  rows <- try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings])
  close conn
  return rows

-- todo: document
updateSettings :: ToRow a => a -> IO ()
updateSettings body = do
  conn <- open dbName
  execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" body
  close conn

-- todo: document
queryHistory :: IO ((Either SQLError [Event]), (Either SQLError [Measurement]))
queryHistory = do
  conn <- open dbName
  events <- try (query_ conn "SELECT * FROM events WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC") :: IO (Either SQLError [Event])
  measurements <- try (query_ conn "SELECT * FROM measurements WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC") :: IO (Either SQLError [Measurement])
  close conn
  return (events, measurements)
