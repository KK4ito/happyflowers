{-|
Module      : Main
Description : Collection of processes that power the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

Starts a REST API server, a WebSockets server and relevant hardware processes.
-}
module Main
    (
      -- * Configuration
      apiPort
    , wsPort
      -- * Operations
    , main
    ) where

import Control.Concurrent                (forkIO)
import HappyFlowers.API.Application
import HappyFlowers.Hardware.Application
import HappyFlowers.WS.Application

-- | 'apiPort' determines which port the API application is run on.
apiPort :: Int
apiPort = 5000

-- | 'wsPort' determines which port the WebSockets application is run on.
wsPort :: Int
wsPort = 9160

-- | The 'startServer' function sets up the API application on 'apiPort' and the
-- WebSockets application on port 'wsPort'.
main :: IO ()
main = do
    putStr "[API] Starting server on port " >> (putStr . show) apiPort >> putStrLn "..."
    forkIO $ apiApp apiPort
    putStr "[WS]  Starting server on port " >> (putStr . show) wsPort >> putStrLn "..."
    forkIO $ wsApp wsPort
    putStr "[HW]  Starting process using port " >> (putStr . show) wsPort >> putStrLn "..."
    hwApp wsPort
