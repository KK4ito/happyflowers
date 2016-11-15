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
module HappyFlowers.Server (
  -- * Applications
  apiApp,
  wsApp,
  -- * Operations
  startServer
  ) where

import Control.Concurrent (forkIO, newMVar)
import HappyFlowers.API.Routes
import HappyFlowers.API.Middlewares
import HappyFlowers.API.WS
import Network.WebSockets
import Web.Scotty

-- todo: improve documentation
apiPort :: Int
apiPort = 5000

-- todo: improve documentation
apiApp :: IO ()
apiApp = scotty apiPort $ do
  middleware corsMiddleware
  middleware staticMiddleware
  middleware rewriteMiddleware
  getSettings >> putSettings >> getHistory >> postAuth >> getRoot

-- todo: improve documentation
wsPort :: Int
wsPort = 9160

-- todo: improve documentation
wsApp :: IO ()
wsApp = do
  state <- newMVar newServerState
  runServer "0.0.0.0" wsPort $ application state

-- | The 'startServer' function sets up a local Scotty server listening on port
-- 5000. It contains several middlewares and reacts to a set of routes.
startServer :: IO ()
startServer = do
  putStrLn "API server running on port 5000..."
  apiApp
  -- putStrLn "WS server running on port 9160..."
  -- wsApp
