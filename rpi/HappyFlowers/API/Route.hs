{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{-|
Module      : HappyFlowers.API.Route
Description : Routing functions for the web server implementation
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to route requests on the server.
-}
module HappyFlowers.API.Route
    (
      -- * Routes
      getSettings
    , putSettings
    , getHistory
    , postAuth
    , getRoot
    ) where

import           Control.Exception         (try)
import           Control.Monad.Trans       (liftIO)
import           Data.Aeson                (FromJSON)
import           Data.ByteString.Char8     (ByteString, pack)
import           Data.Text                 (Text)
import           Database.SQLite.Simple    (ToRow, toRow)
import           GHC.Generics
import           Jose.Jws                  (hmacDecode, hmacEncode)
import           Jose.Jwa                  (JwsAlg(..))
import           Network.HTTP.Types.Status (status401, status500)
import           Web.Scotty

import           HappyFlowers.Config       (getConfig)
import qualified HappyFlowers.DB           as DB

-- | defines the secret string that is used for encrypting and decrypting the
-- JSON Web Token.
tokenSecret :: ByteString
tokenSecret = "hppyflwrs"

-- | handles GET requests for application settings. Produces a JSON response
-- containing settings or an HTTP error if data is missing.
getSettings :: ScottyM ()
getSettings = get "/api/settings/" $ do
    settings <- liftIO DB.querySettings

    case settings of
        Just settings' -> json settings'
        Nothing        -> status status500

-- | 'PutSettingsBody' is used to parse the request body of the 'putSettings'
-- function.
data PutSettingsBody = PutSettingsBody
    { token :: !String     -- ^ Token used for authentication
    , name :: !Text        -- ^ New `name` entry
    , upper :: !Int        -- ^ New `upper` entry
    , lower :: !Int        -- ^ New `lower` entry
    , interval :: !Int     -- ^ New `interval` entry
    } deriving Generic

instance FromJSON PutSettingsBody
instance ToRow PutSettingsBody where
    toRow (PutSettingsBody _ name upper lower interval) = toRow (name, upper, lower, interval)

-- | handles PUT requests for application settings. Requires authentication
-- using JWT. Produces a JSON response containing the new entity or an HTTP
-- error if authentication failed or data could not be stored.
putSettings :: ScottyM ()
putSettings = put "/api/settings/" $ do
    body <- jsonData :: ActionM PutSettingsBody

    let jwt = hmacDecode tokenSecret . pack $ token body
    case jwt of
        Left _  -> status status401
        Right _ -> do
            liftIO $ DB.updateSettings body
            settings <- liftIO DB.querySettings

            case settings of
                Just settings' -> json settings'
                Nothing        -> status status500

-- | handles GET request for historical application data. Produces a JSON
-- response containing the history data or an HTTP error if data could not be
-- retrieved.
getHistory :: ScottyM ()
getHistory = get "/api/history/" $ do
    history <- liftIO DB.queryHistory

    case history of
        Just history' -> json history'
        Nothing       -> status status500

-- | 'PostAuthBody' is used to parse the request body of the 'postAuth'
-- function.
data PostAuthBody = PostAuthBody
    { password :: !Text -- ^ User-submitted password
    } deriving Generic

instance FromJSON PostAuthBody

-- | handles POST requests for authentication. Produces a JSON response
-- containing the JWT or an HTTP error if authentication was not successful.
postAuth :: ScottyM ()
postAuth = post "/api/auth/" $ do
    body <- jsonData :: ActionM PostAuthBody

    let pw = password body
    syspw <- liftIO $ getConfig "password"

    if pw == syspw
        then do
            let jwt = hmacEncode HS384 tokenSecret "hello"
            case jwt of
                Left _     -> status status500
                Right jwt' -> json jwt'
        else do
            status status401

-- | handles GET requests for the root route. Used to serve the JS application.
getRoot :: ScottyM ()
getRoot = get "/" $ do
    file "../web/build/index.html"
