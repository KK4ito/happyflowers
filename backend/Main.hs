{-|
Module      : Main
Description : Collection of processes that power the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module Main
    (
      -- * Operations
      main
    ) where

import Control.Concurrent                (newMVar, forkIO)

import HappyFlowers.API.Application
import HappyFlowers.Hardware.Application
import HappyFlowers.Type                 (BusyState(..))
import HappyFlowers.WS.Application

-- | determines which port the API application is run on.
apiPort :: Int
apiPort = 5000

-- | determines which port the WebSockets application is run on.
wsPort :: Int
wsPort = 9160

newBusyState :: BusyState
newBusyState = Idle

-- | sets up the API application and the WebSockets application. The Hardware
-- processes are started with communication to the WS server.
main :: IO ()
main = do
    busy <- newMVar newBusyState
    putStr "[API] Starting server on port " >> (putStr . show) apiPort >> putStrLn "..."
    forkIO $ apiApp apiPort
    putStr "[WS]  Starting server on port " >> (putStr . show) wsPort >> putStrLn "..."
    forkIO $ wsApp wsPort busy
    putStr "[HW]  Starting process using port " >> (putStr . show) wsPort >> putStrLn "..."
    hwApp wsPort busy
