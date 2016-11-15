{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as C
import Database.SQLite.Simple
import GHC.Generics
import HappyFlowers.API.Config
import HappyFlowers.API.Types
import Jose.Jws
import Jose.Jwa
import Network.HTTP.Types.Status (status500, status401)
import Web.Scotty

-- todo: document
dbName :: String
dbName = "happyflowers.db"

-- todo: document
tokenSecret :: C.ByteString
tokenSecret = "hppyflwrs"

-- | The 'getSettings' function handles GET requests for application settings.
-- The settings are retrieved from the sqlite database. An HTTP error is
-- produced if the settings could not be retrieved or don't contain any values.
getSettings :: ScottyM ()
getSettings = get "/api/settings/" $ do
  conn <- liftIO (open dbName)
  rows <- liftIO (try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings]))
  liftIO (close conn)
  case rows of
    Left _      -> status status500
    Right rows' -> json (head rows')

-- todo: document
data PutSettingsBody =
  PutSettingsBody { token :: String
                  , name :: String
                  , upper :: Int
                  , lower :: Int
                  , interval :: Int
                  } deriving (Generic)
instance FromJSON PutSettingsBody
instance ToRow PutSettingsBody where
  toRow (PutSettingsBody _ name upper lower interval) = toRow (name, upper, lower, interval)

-- | The 'putSettings' function handles PUT requests for application settings.
-- The new data is parsed from the form data passed to the request and is then
-- stored in the database. The request then returns the new entity.
-- This request requires authentication using the JWT. An HTTP error is
-- produced if the request failed to provide authentication. See 'postAuth' for
-- more information.
putSettings :: ScottyM ()
putSettings = put "/api/settings/" $ do
  body <- jsonData :: ActionM PutSettingsBody
  let t = token body
  let jwt = hmacDecode tokenSecret $ C.pack t
  case jwt of
    Left _  -> status status401
    Right _ -> do
      conn <- liftIO (open dbName)
      liftIO (execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" body)
      rows <- liftIO (try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings]))
      liftIO (close conn)
      case rows of
        Left _      -> status status500
        Right rows' -> if length rows' > 0 then json (head rows') else status status500

-- | The 'getHistory' function handles GET request for historical application
-- data. The data is retrieved from the sqlite database.
getHistory :: ScottyM ()
getHistory = get "/api/history/" $ do
  conn <- liftIO (open dbName)
  events <- liftIO (try (query_ conn "SELECT * FROM events WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC") :: IO (Either SQLError [Event]))
  measurements <- liftIO (try (query_ conn "SELECT * FROM measurements WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC") :: IO (Either SQLError [Measurement]))
  liftIO (close conn)
  case (events, measurements) of
    (Right events', Right measurements') -> json History { events = events'
                                                         , measurements = measurements'
                                                         }
    otherwise                            -> status status500

-- todo: improve documentation
data PostAuthBody =
  PostAuthBody { password :: String } deriving (Generic)
instance FromJSON PostAuthBody

-- | The 'postAuth' function handles POST requests for authentication. The
-- user-entered password is parsed from the form data. It is then checked
-- against the password stored in the configuration file. The request returns
-- either a JWT marking successful authentication or produces an HTTP error.
postAuth :: ScottyM ()
postAuth = post "/api/auth/" $ do
  body <- jsonData :: ActionM PostAuthBody
  let pw = password body
  syspw <- liftIO getPassword
  if pw == syspw
    then do
      let jwt = hmacEncode HS384 tokenSecret "hello"
      case jwt of
        Left _     -> status status500
        Right jwt' -> json jwt'
    else do
      status status401

-- | The 'getRoot' handles GET requests for the root route. This is used to
-- serve the web front end. All non-API requests are rewritten to this route.
getRoot :: ScottyM ()
getRoot = get "/" $ do
  file "../web/build/index.html"
