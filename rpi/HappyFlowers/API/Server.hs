{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.API.Server
Description : Scotty web server implementation for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

An implementation of the Scotty web framework offering a RESTful API and serving
static files.
-}
module HappyFlowers.API.Server (
  -- * Applications
  apiApp,
  wsApp,
  -- * Operations
  startServer
  ) where

import Data.Text (Text)
import HappyFlowers.API.Routes
import HappyFlowers.API.Middlewares
import Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.Routes
import Network.WebSockets

-- todo: improve documentation
apiApp :: RouteM ()
apiApp = do
  middleware corsMiddleware
  middleware staticMiddleware
  -- TODO: fix
  -- middleware rewriteMiddleware
  route AppRoute

-- todo: improve documentation
wsApp :: ServerApp
wsApp pending_conn = do
  conn <- acceptRequest pending_conn
  sendTextData conn ("{ \"type\": \"hello\", \"payload\": { \"data\": \"world\" } }" :: Text)

-- | The 'startServer' function sets up a local Scotty server listening on port
-- 5000. It contains several middlewares and reacts to a set of routes.
startServer :: IO ()
startServer = do
  putStrLn "Running server on port 5000..."
  run 5000 $ websocketsOr defaultConnectionOptions wsApp (waiApp apiApp)
