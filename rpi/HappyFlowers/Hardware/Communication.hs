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
module HappyFlowers.Hardware.Communication
    (
      -- * Sensor
      checkMoisture
      -- * Pump
    , activatePump
    ) where

import           Control.Concurrent           (threadDelay)
import           Data.Aeson                   (ToJSON, encode, object, (.=))
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy.Char8   as CL
import qualified Data.Text                    as T
import           Database.SQLite.Simple.Types (Only(..))
import qualified Network.WebSockets           as WS
import qualified System.RaspberryPi.GPIO      as G

import qualified HappyFlowers.DB              as DB
import           HappyFlowers.Type            (interval, lower, upper)

-- | determines the address of the port that is used to read data.
address :: G.Address
address = 0x20

-- | reads data from the chirp sensor.
readMoisture :: G.Address -> IO ()
readMoisture address = G.withGPIO . G.withI2C $ G.readI2C address 0 >>= putStrLn . C.unpack

-- Only used during development.
mockMoisture :: IO Int
mockMoisture = putStrLn "sensor on" >> threadDelay 3000000 >> putStrLn "sensor off" >> return 80

-- | sends WS notifications to all connected clients about measurements or
-- events if a payload is available.
notify :: ToJSON a => WS.Connection -> T.Text -> Maybe a -> IO ()
notify conn kind = maybe (return ()) notify'
    where
        notify' p = WS.sendTextData conn $ T.concat [ "{ \"type\": \""
                                                    , kind
                                                    , "\", \"payload\":"
                                                    , (T.pack . CL.unpack $ encode p), "}"
                                                    ]

-- | checks the plant's moisutre level and informs connected clients about the
-- measurement. Triggers the pump if the lower moisture limit is reached.
checkMoisture :: WS.Connection -> IO ()
checkMoisture conn = do
    settings <- DB.querySettings

    case settings of
        Just settings' -> do
            d <- mockMoisture
            DB.addMeasurement d
            DB.queryLatestMeasurement >>= notify conn "measurementReceived"

            -- Either activate the pump or schedule another measurement after the
            -- defined interval.

            if d < (lower settings')
                then do
                    activatePump conn
                else do
                    threadDelay $ (interval settings') * 60000000
                    checkMoisture conn
        Nothing        -> return ()

-- | activates the pump and keeps repeating until the upper moisture limit is
-- reached. Informs all connected clients about the watering.
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
                    DB.queryLatestEvent >>= notify conn "eventReceived"
                else do
                    activatePump conn
        Nothing        -> return ()

-- Only used during development.
mockTriggerPump :: IO ()
mockTriggerPump = putStrLn "pump on" >> threadDelay 5000000 >> putStrLn "pump off"
