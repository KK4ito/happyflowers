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

import Control.Concurrent (forkIO, newMVar, threadDelay)
import Control.Monad (forever)
import HappyFlowers.API.Application
import HappyFlowers.WS.Application
import Network.WebSockets
import Web.Scotty

-- todo: improve documentation
apiPort :: Int
apiPort = 5000

-- todo: improve documentation
wsPort :: Int
wsPort = 9160

-- todo: improve documentation
startServer :: IO ()
startServer = do
  putStr "[API] Starting server on port " >> (putStr . show) apiPort >> putStrLn "..."
  forkIO apiApp apiPort
  putStr "[WS]  Starting server on port " >> (putStr . show) wsPort >> putStrLn "..."
  forkIO wsApp wsPort
  forever $ threadDelay 100000
