{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{-|
Module      : HappyFlowers.API.Route
Description : Routing functions for the web server implementation
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
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
import           Data.Aeson                (ToJSON, FromJSON, object, (.=))
import           Data.ByteString.Char8     (ByteString, pack)
import           Data.Text                 (Text)
import           Database.SQLite.Simple    (ToRow, toRow)
import           GHC.Generics
import           Jose.Jws                  (hmacDecode, hmacEncode)
import           Jose.Jwa                  (JwsAlg(..))
import           Network.HTTP.Types.Status (Status, status401, status500)
import           Web.Scotty

import           HappyFlowers.Config       (getConfig)
import qualified HappyFlowers.DB           as DB

-- | defines the secret string that is used for encrypting and decrypting the
-- JSON Web Token.
tokenSecret :: ByteString
tokenSecret = "hppyflwrs"

-- | produces a JSON response or an HTTP error based on a given payload.
jsonOrError :: ToJSON t
            => Status  -- Status if not erroneous
            -> Maybe t -- Payload to evaluate
            -> ActionM ()
jsonOrError s = maybe (status s) json

-- | handles GET requests for application settings. Produces a JSON response
-- containing settings or an HTTP error if data is missing.
getSettings :: ScottyM ()
getSettings = get "/api/settings/" $ liftIO DB.querySettings >>= jsonOrError status500

-- | PutSettingsBody is used to parse the request body of the 'putSettings'
-- function.
data PutSettingsBody = PutSettingsBody
    { token :: !String     -- ^ Token used for authentication
    , name :: !Text        -- ^ Flower name
    , upper :: !Int        -- ^ Upper moisture limit
    , lower :: !Int        -- ^ Lower moisture limit
    , interval :: !Int     -- ^ Measurement interval
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
            s <- liftIO DB.querySettings
            jsonOrError status500 s

-- | handles GET request for historical application data. Produces a JSON
-- response containing the history data or an HTTP error if data could not be
-- retrieved.
getHistory :: ScottyM ()
getHistory = get "/api/history/" $ liftIO DB.queryHistory >>= jsonOrError status500

-- | PostAuthBody is used to parse the request body of the 'postAuth' function.
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
            let token = hmacEncode HS384 tokenSecret "hello"
            either (\_ -> status status500) (\t -> json $ object [ "token" .= t ]) token
        else status status401

-- | handles GET requests for the root route. Used to serve the JS application.
getRoot :: ScottyM ()
getRoot = get "/" $ do
    file "../web/build/index.html"
