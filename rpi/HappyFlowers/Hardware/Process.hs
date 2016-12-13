{-# LANGUAGE OverloadedStrings, CPP #-}

{-|
Module      : HappyFlowers.Hardware.Process
Description : Communicate with the RPi's hardware
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Hardware.Process
    (
      -- * Operations
      client
    ) where

import           Control.Concurrent               (MVar, modifyMVar, readMVar, forkIO)
import           Control.Concurrent.Thread.Delay  (delay)
import           Control.Monad                    (forever, when, unless)
import           Control.Monad.Trans              (liftIO)
import qualified Data.Text                        as T
import qualified Network.WebSockets               as WS

import qualified HappyFlowers.DB                  as DB
import           HappyFlowers.Type                (BusyState(..), Settings(..), WSEventKind(..), MeasurementKind(..), EventKind(..))
import           HappyFlowers.WS.Communication    (notify)

#ifdef Development
import qualified HappyFlowers.Hardware.DeviceMock as HW
#else
import qualified HappyFlowers.Hardware.Device     as HW
#endif

delayByInterval :: Int -- Delay in minutes
                -> IO ()
delayByInterval i = delay . (60000000 *) $ toInteger i

updateBusy :: WS.Connection -> MVar BusyState -> BusyState -> IO ()
updateBusy conn busy state = do
    b <- modifyMVar busy $ \s -> return (state, state)
    notify conn BusyChanged $ b == Busy

saveMeasurement :: WS.Connection -> MeasurementKind -> Int -> IO ()
saveMeasurement conn kind value = do
    DB.addMeasurement kind value
    m <- DB.queryLatestMeasurement kind
    notify conn MeasurementReceived m

saveEvent :: WS.Connection -> EventKind -> IO ()
saveEvent conn kind = do
    DB.addEvent kind
    e <- DB.queryLatestEvent
    notify conn EventReceived e

-- | checks the plant's moisutre level and informs connected clients about the
-- measurement. Triggers the pump if the lower moisture limit is reached.
checkMoisture :: WS.Connection -> MVar BusyState -> IO ()
checkMoisture conn busy = withSettings $ \settings' -> do
    b <- readMVar busy

    unless (b == Busy) $ do
        updateBusy conn busy Busy

        m <- HW.readMoisture
        saveMeasurement conn Moisture m

        t <- HW.readTemperature
        saveMeasurement conn Temperature t

        if m < lower settings'
            then do
                activatePump conn busy
            else do
                updateBusy conn busy Idle

    delayByInterval $ interval settings'
    checkMoisture conn busy

-- | activates the pump and keeps repeating until the upper moisture limit is
-- reached. Informs all connected clients about the watering.
activatePump :: WS.Connection -> MVar BusyState -> IO ()
activatePump conn busy = withSettings $ \settings' -> do
    HW.triggerPump

    m <- HW.readMoisture

    if m >= upper settings'
        then do
            saveMeasurement conn Moisture m

            t <- HW.readTemperature
            saveMeasurement conn Temperature t

            saveEvent conn Automatic
            updateBusy conn busy Idle
        else do
            activatePump conn busy

-- | activates the pump based on a user interaction if the flower is not already
-- fully moisturised. Informs all connected clients about the watering.
manualPump :: WS.Connection -> MVar BusyState -> IO ()
manualPump conn busy = withSettings $ \settings' -> do
    b <- readMVar busy

    unless (b == Busy) $ do
        updateBusy conn busy Busy

        m <- HW.readMoisture
        saveMeasurement conn Moisture m

        t <- HW.readTemperature
        saveMeasurement conn Temperature t

        unless (m >= upper settings') $ do
            HW.triggerPump

            m <- HW.readMoisture
            saveMeasurement conn Moisture m

            saveEvent conn Manual
    updateBusy conn busy Idle

-- | retrieves settings from the database and handles them using a given action
-- if a value could be retrieved.
withSettings :: (Settings -> IO ()) -> IO ()
withSettings action = DB.querySettings >>= maybe (return ()) action

-- | creates a new WS client application that checks plant moisture on a regular
-- basis.
client :: MVar BusyState -> WS.ClientApp ()
client busy conn = do
    forkIO . forever $ do
        msg <- WS.receiveData conn
        liftIO . when ((T.pack $ show TriggerPump) `T.isInfixOf` msg) $ manualPump conn busy

    liftIO $ checkMoisture conn busy
