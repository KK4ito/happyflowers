{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.RPI.Communication
Description : Communicate with the RPi's hardware
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to communicate with the RPi's
hardware and connected sensors and devices.
-}
module HappyFlowers.Hardware.Communication (
  -- * Configuration
  address,
  -- * Operations
  readMoisture,
  mockMoisture,
  checkMoisture
  ) where

import           Control.Concurrent           (threadDelay)
import           Data.Aeson                   (encode, object, (.=))
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy.Char8   as CL
import qualified Data.Text                    as T
import           Database.SQLite.Simple.Types (Only(..))
import qualified HappyFlowers.DB              as DB
import           HappyFlowers.Types
import qualified Network.WebSockets           as WS
import           System.RaspberryPi.GPIO

-- | 'address' determines the address of the port that is used to read data.
address :: Address
address = 0x20

-- | The 'readMoisture' function reads data from the chirp sensor through the
-- 'address'. It relies on GPIO and I2C.
readMoisture :: IO ()
readMoisture = withGPIO . withI2C $ do
  d <- readI2C address 0
  putStrLn $ C.unpack d

-- TODO: document
mockMoisture :: IO Int
mockMoisture = do
  putStrLn "sensor on"
  threadDelay 3000000
  putStrLn "sensor off"
  return 80

-- TODO: document
checkMoisture :: WS.Connection -> IO ()
checkMoisture conn = do
  settings <- DB.querySettings
  case settings of
    Just settings' -> do
      d <- mockMoisture
      DB.addMeasurement d

      me <- DB.queryLatestMeasurement
      case me of
        Just me' -> WS.sendTextData conn $ T.concat [ "{ \"type\": \"measurementReceived\", \"payload\":", (T.pack $ CL.unpack $ encode me'), "}" ]

      -- Either activate the pump or schedule another measurement after the
      -- defined interval.

      if d < (lower settings')
        then do
          activatePump conn
        else do
          threadDelay $ (interval settings') * 60000000
          checkMoisture conn

-- TODO: document
activatePump :: WS.Connection -> IO ()
activatePump conn = do
  mockTriggerPump
  settings <- DB.querySettings
  case settings of
    Just settings' -> do
      d <- mockMoisture

      if d >= (upper settings')
        then do
          DB.addEvent "automatic"
          ev <- DB.queryLatestEvent
          case ev of
            Just ev' -> WS.sendTextData conn $ T.concat [ "{ \"type\": \"eventReceived\", \"payload\":", (T.pack $ CL.unpack $ encode ev'), "}" ]
        else do
          activatePump conn

-- TODO: document
mockTriggerPump :: IO ()
mockTriggerPump = do
  putStrLn "pump on"
  threadDelay 5000000
  putStrLn "pump off"
