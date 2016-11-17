{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.Server
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
  -- * Configuration
  apiPort,
  wsPort,
  -- * Operations
  startServer
  ) where

import Control.Concurrent (forkIO)
import HappyFlowers.API.Application
import HappyFlowers.WS.Application

-- | 'apiPort' determines which port the API application is run on.
apiPort :: Int
apiPort = 5000

-- | 'wsPort' determines which port the WebSockets application is run on.
wsPort :: Int
wsPort = 9160

-- | The 'startServer' function sets up the API application on 'apiPort' and the
-- WebSockets application on port 'wsPort'.
startServer :: IO ()
startServer = do
  putStr "[API] Starting server on port " >> (putStr . show) apiPort >> putStrLn "..."
  forkIO $ apiApp apiPort
  putStr "[WS]  Starting server on port " >> (putStr . show) wsPort >> putStrLn "..."
  wsApp wsPort
