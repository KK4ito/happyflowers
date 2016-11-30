{-|
Module      : HappyFlowers.RPI.Application
Description : WebSockets client implementation that communicates with hardware
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

A WebSockets client implementation that communicates with the RPi's hardware.
-}
module HappyFlowers.Hardware.Application
    (
      -- * Operations
      hwApp
    ) where

import           Control.Monad.Trans                 (liftIO)
import           Network.Socket                      (withSocketsDo)
import qualified Network.WebSockets                  as WS

import qualified HappyFlowers.Hardware.Communication as C

-- | creates a new WS client application that checks plant moisture on a regular
-- basis.
client :: WS.ClientApp ()
client conn = do
    liftIO $ C.checkMoisture conn

-- | sets up a WS connection to the WS server listening on the given port.
hwApp :: Int -- ^ Port
      -> IO ()
hwApp port = withSocketsDo $ WS.runClient "0.0.0.0" port "/" client
