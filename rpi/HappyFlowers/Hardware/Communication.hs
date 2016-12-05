{-# LANGUAGE OverloadedStrings, CPP #-}

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
      -- * Operations
      client
    ) where

import           Control.Concurrent              (forkIO)
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad                   (forever, when, unless)
import           Control.Monad.Trans             (liftIO)
import           Data.Aeson                      (ToJSON, encode)
import qualified Data.ByteString.Char8           as C
import qualified Data.ByteString.Lazy.Char8      as CL
import qualified Data.Text                       as T
import qualified Network.WebSockets              as WS
import           System.RaspberryPi.GPIO         (Address, withGPIO, setPinFunction, writePin, Pin(..), PinMode(..), withI2C, readI2C)

import qualified HappyFlowers.DB                 as DB
import           HappyFlowers.Type               (Settings, busy, interval, lower, upper)

-- | determines the address of the port that is used to read data.
address :: Address
address = 0x20

-- | reads data from the chirp sensor.
#ifdef Development
readMoisture :: Int -> IO Int
readMoisture value = putStrLn "sensor on" >> delay 3000000 >> putStrLn "sensor off" >> return value
#else
readMoisture :: IO Int
readMoisture = withGPIO . withI2C $ readI2C address 0 >>= \val -> return $ read (C.unpack val) :: IO Int
#endif

-- | triggers the USB water pump.
#ifdef Development
triggerPump :: IO ()
triggerPump = putStrLn "pump on" >> delay 5000000 >> putStrLn "pump off"
#else
triggerPump :: IO ()
triggerPump = withGPIO $ do
    setPinFunction Pin07 Output
    writePin Pin07 True
    delay 5000000
    writePin Pin07 False
#endif

-- | sends WS notifications to all connected clients about measurements or
-- events if a payload is available.
notify :: ToJSON a
       => WS.Connection -- Connection to send data to
       -> T.Text        -- WS event type
       -> Maybe a       -- Payload
       -> IO ()
notify conn kind = maybe (return ()) notify'
    where
        notify' p = WS.sendTextData conn $ T.concat [ "{ \"type\": \""
                                                    , kind
                                                    , "\", \"payload\":"
                                                    , (T.pack . CL.unpack $ encode p)
                                                    , "}"
                                                    ]

-- | checks the plant's moisutre level and informs connected clients about the
-- measurement. Triggers the pump if the lower moisture limit is reached.
checkMoisture :: WS.Connection -> IO ()
checkMoisture conn = handle $ \settings' -> do
    if not (busy settings')
        then do
            DB.updateBusy 1
            WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": true }" :: T.Text)

#ifdef Development
            d <- readMoisture 30
#else
            d <- readMoisture
#endif

            DB.addMeasurement d
            DB.queryLatestMeasurement >>= notify conn "measurementReceived"

            if d < lower settings'
                then do
                    activatePump conn
                else do
                    DB.updateBusy 0
                    WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": false }" :: T.Text)
                    delay . (60000000 *) . toInteger $ interval settings'
                    checkMoisture conn
        else do
            delay . (60000000 *) . toInteger $ interval settings'
            checkMoisture conn

-- | activates the pump and keeps repeating until the upper moisture limit is
-- reached. Informs all connected clients about the watering.
activatePump :: WS.Connection -> IO ()
activatePump conn = handle $ \settings' -> do
    triggerPump
    delay 5000000

#ifdef Development
    d <- readMoisture 80
#else
    d <- readMoisture
#endif

    if d >= upper settings'
        then do
            DB.addEvent "automatic"
            DB.queryLatestEvent >>= notify conn "eventReceived"

            DB.updateBusy 0
            WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": false }" :: T.Text)

#ifdef Development
            DB.addMeasurement d
            DB.queryLatestMeasurement >>= notify conn "measurementReceived"

            delay . (60000000 *) . toInteger $ interval settings'
            checkMoisture conn
#endif

        else do
            activatePump conn

-- | activates the pump based on a user interaction if the flower is not already
-- fully moisturised. Informs all connected clients about the watering.
manualPump :: WS.Connection -> IO ()
manualPump conn = handle $ \settings' -> do
    unless (busy settings') $ do
        DB.updateBusy 1
        WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": true }" :: T.Text)

#ifdef Development
        d <- readMoisture 50
#else
        d <- readMoisture
#endif

        DB.addMeasurement d
        DB.queryLatestMeasurement >>= notify conn "measurementReceived"

        if (d < upper settings')
            then do
                triggerPump
                delay 5000000

                DB.addEvent "manual"
                DB.queryLatestEvent >>= notify conn "eventReceived"

#ifdef Development
                readMoisture 80 >>= DB.addMeasurement
#else
                readMoisture >>= DB.addMeasurement
#endif

                DB.updateBusy 0
                WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": false }" :: T.Text)
                DB.queryLatestMeasurement >>= notify conn "measurementReceived"
            else do
                DB.updateBusy 0
                WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": false }" :: T.Text)

-- | retrieves settings from the database and handles them using a given action
-- if a value could be retrieved.
handle :: (Settings -> IO ()) -> IO ()
handle action = DB.querySettings >>= maybe (return ()) action

-- | creates a new WS client application that checks plant moisture on a regular
-- basis.
client :: WS.ClientApp ()
client conn = do
    forkIO . forever $ do
        msg <- WS.receiveData conn
        liftIO . when ("triggerPump" `T.isInfixOf` msg) $ manualPump conn

#ifdef Development
    liftIO $ checkMoisture conn
#else
    forever $ checkMoisture conn
#endif
