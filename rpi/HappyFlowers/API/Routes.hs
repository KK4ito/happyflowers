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
  -- * Configuration
  tokenSecret,
  -- * Routes
  getSettings,
  putSettings,
  getHistory,
  postAuth,
  getRoot
  ) where

import           Control.Exception
import           Control.Monad.Trans (liftIO)
import           Data.Aeson (FromJSON)
import           Database.SQLite.Simple (ToRow, toRow)
import qualified Data.ByteString.Char8 as C
import           GHC.Generics
import           HappyFlowers.API.Config
import           HappyFlowers.Types
import qualified HappyFlowers.DB as DB
import           Jose.Jws
import           Jose.Jwa
import           Network.HTTP.Types.Status (status401, status500)
import           Web.Scotty

-- | 'tokenSecret' defines the secret string that is used for the JSON Web
-- Token. It is used to encrypt and decrypt the token.
tokenSecret :: C.ByteString
tokenSecret = "hppyflwrs"

-- | The 'getSettings' function handles GET requests for application settings.
-- The settings are retrieved from the sqlite database. An HTTP error is
-- produced if the settings could not be retrieved.
getSettings :: ScottyM ()
getSettings = get "/api/settings/" $ do
  rows <- liftIO DB.querySettings
  case rows of
    Left _      -> status status500
    Right rows' -> json $ head rows'

-- | 'PutSettingsBody' defines the type that is used to parse the request body
-- of the 'putSettings' function. It can be parsed from scotty's jsonData method
-- and converted to a sqlite row instance so it can be stored in the database.
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
  let jwt = hmacDecode tokenSecret $ C.pack $ token body
  case jwt of
    Left _  -> status status401
    Right _ -> do
      liftIO $ DB.updateSettings body
      rows <- liftIO DB.querySettings
      case rows of
        Left _      -> status status500
        Right rows' -> json $ head rows'

-- | The 'getHistory' function handles GET request for historical application
-- data. The data is retrieved from the sqlite database.
getHistory :: ScottyM ()
getHistory = get "/api/history/" $ do
  history <- liftIO DB.queryHistory
  case history of
    (Right events', Right measurements') -> json History { events = events'
                                                         , measurements = measurements'
                                                         }
    otherwise                            -> status status500

-- | 'PostAuthBody' defines the type that is used to parse the request body of
-- the 'postAuth' function. It can be parsed from scotty's jsonData method.
data PostAuthBody =
  PostAuthBody { password :: String
               } deriving (Generic)
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

  case () of
    _ | pw == syspw -> do
        let jwt = hmacEncode HS384 tokenSecret "hello"
        case jwt of
          Left _     -> status status500
          Right jwt' -> json jwt'
      | otherwise   -> status status401

-- | The 'getRoot' handles GET requests for the root route. This is used to
-- serve the web front end. All non-API requests are rewritten to this route.
getRoot :: ScottyM ()
getRoot = get "/" $ do
  file "../web/build/index.html"
