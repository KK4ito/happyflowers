{-|
Module      : HappyFlowers.RPI.Application
Description : WebSockets client implementation that communicates with hardware
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Hardware.Application
    (
      -- * Operations
      hwApp
    ) where

import Control.Concurrent            (MVar)
import Network.Socket                (withSocketsDo)
import Network.WebSockets            (runClient)

import HappyFlowers.Hardware.Process (client)
import HappyFlowers.Type             (BusyState(..))

-- | sets up a WS connection to the WS server listening on the given port.
hwApp :: Int -- ^ Port
      -> MVar BusyState
      -> IO ()
hwApp port busy = withSocketsDo . runClient "0.0.0.0" port "/" $ client busy
