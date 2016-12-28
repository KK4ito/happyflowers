{-# LANGUAGE OverloadedStrings, CPP #-}

{-|
Module      : HappyFlowers.Hardware.Process
Description : Handle processes like measurements and waterings
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Hardware.Process
    (
      -- * Operations
      checkMoisture
    , activatePump
    , manualPump
    ) where

import           Control.Concurrent               (MVar, readMVar)
import           Control.Concurrent.Thread.Delay  (delay)
import           Control.Monad                    (unless)

import           HappyFlowers.Type                (BusyState(..), Settings(..), WSEventKind(..), MeasurementKind(..), EventKind(..), Command(..))

#ifdef Development
import qualified HappyFlowers.Hardware.DeviceMock as HW
#else
import qualified HappyFlowers.Hardware.Device     as HW
#endif

-- | delays the current thread by a given duration.
delayByInterval :: Int -- Delay in minutes
                -> IO ()
delayByInterval i = delay . (60000000 *) $ toInteger i

-- | checks the plant's moisutre level and informs connected clients about the
-- measurement. Triggers the pump if the lower moisture limit is reached.
checkMoisture :: Settings -> MVar BusyState -> (Command -> IO ()) -> IO ()
checkMoisture settings busy callback = do
    b <- readMVar busy

    unless (b == Busy) $ do
        callback $ UpdateBusy Busy

        m <- HW.readMoisture
        callback $ SaveMeasurement Moisture m

        t <- HW.readTemperature
        callback $ SaveMeasurement Temperature t

        if m < lower settings
            then do
                callback PumpRequired
            else do
                callback $ UpdateBusy Idle

    delayByInterval $ interval settings
    callback CheckRequired

-- | activates the pump and keeps repeating until the upper moisture limit is
-- reached. Informs all connected clients about the watering.
activatePump :: Settings -> (Command -> IO ()) -> IO ()
activatePump settings callback = do
    HW.triggerPump

    m <- HW.readMoisture

    if m >= upper settings
        then do
            callback $ SaveEvent Automatic
            callback CheckRequired
        else do
            callback PumpRequired

-- | activates the pump based on a user interaction if the flower is not already
-- fully moisturised. Informs all connected clients about the watering.
manualPump :: Settings -> MVar BusyState -> (Command -> IO ()) -> IO ()
manualPump settings busy callback = do
    b <- readMVar busy

    unless (b == Busy) $ do
        callback $ UpdateBusy Busy

        m <- HW.readMoisture
        callback $ SaveMeasurement Moisture m

        t <- HW.readTemperature
        callback $ SaveMeasurement Temperature t

        unless (m >= upper settings) $ do
            HW.triggerPump

            m <- HW.readMoisture
            callback $ SaveMeasurement Moisture m

            callback $ SaveEvent Manual
    callback $ UpdateBusy Idle
