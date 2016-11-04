{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, RankNTypes #-}

{-|
Module      : HappyFlowers.API.Routes
Description : Routing functions for the web server implementation
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to route requests on the server.
-}
module HappyFlowers.API.Routes (
  -- * Routes
  AppRoute(..),
  getSettingsR,
  getHistoryR,
  getRootR
  ) where

import Control.Monad.Trans (liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as C
import Database.SQLite.Simple
import HappyFlowers.API.Config
import HappyFlowers.API.Types
import Jose.Jws
import Jose.Jwa
import Network.HTTP.Types.Status (status200, status500, status401)
import Network.Wai
import Network.Wai.Middleware.Routes hiding (head)

data AppRoute = AppRoute

mkRoute "AppRoute" [parseRoutes|
/              RootR     GET
/api/settings/ SettingsR GET
/api/history/  HistoryR  GET
|]

-- | The 'getSettings' function handles GET requests for application settings.
-- The settings are retrieved from the sqlite database. An HTTP error is
-- produced if the settings could not be retrieved or don't contain any values.
getSettingsR :: Handler AppRoute
getSettingsR = runHandlerM $ do
  conn <- liftIO (open "happyflowers.db")
  rows <- liftIO (query_ conn "SELECT * FROM settings" :: IO [Settings])
  liftIO (close conn)
  json (head rows)

-- | The 'putSettings' function handles PUT requests for application settings.
-- The new data is parsed from the form data passed to the request and is then
-- stored in the database. The request then returns the new entity.
-- This request requires authentication using the JWT. An HTTP error is
-- produced if the request failed to provide authentication. See 'postAuth' for
-- more information.
{-putSettingsR :: Handler AppRoute
putSettingsR = runHandlerM $ do
  token <- "abc" -- todo: get param from request
  let jwt = hmacDecode "hppyflwrs" $ C.pack token
  case jwt of
    Right _ -> do
      name <- "cecelia" -- todo: get param from request
      upper <- 80 -- todo: get param from request
      lower <- 40 -- todo: get param from request
      interval <- 60 -- todo: get param from request
      conn <- liftIO (open "happyflowers.db")
      liftIO (execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" (name, upper, lower, interval))
      liftIO (close conn)
      json Settings { name = name, upper = upper, lower = lower, interval = interval }
    Left e -> status status401-}

-- | The 'getHistory' function handles GET request for historical application
-- data. The data is retrieved from the sqlite database.
getHistoryR :: Handler AppRoute
getHistoryR = runHandlerM $ do
  conn <- liftIO (open "happyflowers.db")
  events <- liftIO (query_ conn "SELECT * FROM events WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC" :: IO [Event])
  measurements <- liftIO (query_ conn "SELECT * FROM measurements WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC" :: IO [Measurement])
  liftIO (close conn)
  json History { events = events, measurements = measurements }

-- | The 'postAuth' function handles POST requests for authentication. The
-- user-entered password is parsed from the form data. It is then checked
-- against the password stored in the configuration file. The request returns
-- either a JWT marking successful authentication or produces an HTTP error.
{-postAuthR :: Handler AppRoute
postAuthR = runHandlerM $ do
  pw <- "abc" -- todo: get param from request
  syspw <- liftIO getPassword
  if pw == syspw
    then do
      let jwt = hmacEncode HS384 "hppyflwrs" "hello"
      case jwt of
        Right jwt' -> do
          json jwt'
    else do
      status status401-}

-- | The 'getRoot' function handles GET requests for the root route. This is
-- used to serve the web front end. All non-API requests are rewritten to this
-- route.
getRootR :: Handler AppRoute
getRootR = runHandlerM $ do
  file "../web/build/index.html"
