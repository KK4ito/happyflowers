{-# LANGUAGE OverloadedStrings #-}

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
  getSettings,
  putSettings,
  getHistory,
  postAuth,
  getRoot
  ) where

import Control.Exception
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as C
import Database.SQLite.Simple
import HappyFlowers.API.Config
import HappyFlowers.API.Types
import Jose.Jws
import Jose.Jwa
import Network.HTTP.Types.Status (ok200, internalServerError500, unauthorized401)
import Web.Scotty

-- | The 'getSettings' function handles GET requests for application settings.
-- The settings are retrieved from the sqlite database. An HTTP error is
-- produced if the settings could not be retrieved or don't contain any values.
getSettings :: ScottyM ()
getSettings = get "/api/settings/" $ do
  conn <- liftIO (open "happyflowers.db")
  rows <- liftIO (try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings]))
  case rows of
    Left e -> status internalServerError500
    Right r -> if length r > 0 then json (head r) else status internalServerError500
  liftIO (close conn)

-- | The 'putSettings' function handles PUT requests for application settings.
-- The new data is parsed from the form data passed to the request and is then
-- stored in the database. The request then returns the new entity.
-- This request requires authentication using the JWT. An HTTP error is
-- produced if the request failed to provide authentication. See 'postAuth' for
-- more information.
putSettings :: ScottyM ()
putSettings = put "/api/settings/" $ do
  token <- (param "token") :: ActionM String
  let jwt = hmacDecode "hppyflwrs" $ C.pack token
  case jwt of
    Right _ -> do
      name <- (param "name") :: ActionM String
      upper <- (param "upper") :: ActionM Int
      lower <- (param "lower") :: ActionM Int
      interval <- (param "interval" ) :: ActionM Int
      conn <- liftIO (open "happyflowers.db")
      liftIO (execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" (name, upper, lower, interval))
      json Settings { name = name, upper = upper, lower = lower, interval = interval }
      liftIO (close conn)
    Left e -> status unauthorized401

-- | The 'getHistory' function handles GET request for historical application
-- data. The data is retrieved from the sqlite database.
getHistory :: ScottyM ()
getHistory = get "/api/history/" $ do
  conn <- liftIO (open "happyflowers.db")
  events <- liftIO (query_ conn "SELECT * FROM events WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC" :: IO [Event])
  measurements <- liftIO (query_ conn "SELECT * FROM measurements WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC" :: IO [Measurement])
  json History { events = events, measurements = measurements }
  liftIO (close conn)

-- | The 'postAuth' function handles POST requests for authentication. The
-- user-entered password is parsed from the form data. It is then checked
-- against the password stored in the configuration file. The request returns
-- either a JWT marking successful authentication or produces an HTTP error.
postAuth :: ScottyM ()
postAuth = post "/api/auth/" $ do
  pw <- (param "password") :: ActionM String
  syspw <- liftIO getPassword
  if pw == syspw
    then do
      let jwt = hmacEncode HS384 "hppyflwrs" "hello"
      case jwt of
        Right j -> do
          json j
    else do
      status unauthorized401

-- | The 'getRoot' handles GET requests for the root route. This is used to
-- serve the web front end. All non-API requests are rewritten to this route.
getRoot :: ScottyM ()
getRoot = get "/" $ do
  file "../web/build/index.html"
