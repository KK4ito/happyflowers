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

import           Control.Concurrent              (forkIO)
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad                   (forever, when, unless)
import           Control.Monad.Trans             (liftIO)
import           Data.Aeson                      (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8      as CL
import qualified Data.Text                       as T
import qualified Network.WebSockets              as WS
import           System.Random                   (getStdRandom, randomR)

import qualified HappyFlowers.DB                 as DB
import           HappyFlowers.Type               (Settings, busy, interval, lower, upper)

#ifdef Development
import qualified HappyFlowers.Hardware.Mock      as HW
#else
import qualified HappyFlowers.Hardware.Device    as HW
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

delayByInterval :: Int -> IO ()
delayByInterval i = delay . (60000000 *) $ toInteger i

updateBusy :: WS.Connection -> Bool -> IO ()
updateBusy conn state = do
    DB.updateBusy $ if state then 1 else 0
    WS.sendTextData conn ("{ \"type\": \"busy\", \"payload\": " `mappend` (if state then "true" else "false") `mappend` " }" :: T.Text)

saveMeasurement :: WS.Connection -> T.Text -> Int -> IO ()
saveMeasurement conn kind value = do
    DB.addMeasurement kind value
    m <- DB.queryLatestMeasurement kind
    notify conn "measurementReceived" m

saveEvent :: WS.Connection -> T.Text -> IO ()
saveEvent conn kind = do
    DB.addEvent kind
    e <- DB.queryLatestEvent
    notify conn "eventReceived" e

-- | checks the plant's moisutre level and informs connected clients about the
-- measurement. Triggers the pump if the lower moisture limit is reached.
checkMoisture :: WS.Connection -> IO ()
checkMoisture conn = withSettings $ \settings' -> do
    unless (busy settings') $ do
        updateBusy conn True

        m <- HW.readMoisture
        saveMeasurement conn "moisture" m

        t <- HW.readTemperature
        saveMeasurement conn "temperature" t

        if m < lower settings'
            then do
                activatePump conn
            else do
                updateBusy conn False
    delayByInterval $ interval settings'
    checkMoisture conn

-- | activates the pump and keeps repeating until the upper moisture limit is
-- reached. Informs all connected clients about the watering.
activatePump :: WS.Connection -> IO ()
activatePump conn = withSettings $ \settings' -> do
    HW.triggerPump

    m <- HW.readMoisture

    if m >= upper settings'
        then do
            saveMeasurement conn "moisture" m

            t <- HW.readTemperature
            saveMeasurement conn "temperature" t

            saveEvent conn "automatic"
            updateBusy conn False
        else do
            activatePump conn

-- | activates the pump based on a user interaction if the flower is not already
-- fully moisturised. Informs all connected clients about the watering.
manualPump :: WS.Connection -> IO ()
manualPump conn = withSettings $ \settings' -> do
    updateBusy conn True

    unless (busy settings') $ do
        m <- HW.readMoisture
        saveMeasurement conn "moisture" m

        t <- HW.readTemperature
        saveMeasurement conn "temperature" t

        unless (m >= upper settings') $ do
            HW.triggerPump

            m <- HW.readMoisture
            saveMeasurement conn "moisture" m

            saveEvent conn "manual"

    updateBusy conn False

-- | retrieves settings from the database and handles them using a given action
-- if a value could be retrieved.
withSettings :: (Settings -> IO ()) -> IO ()
withSettings action = DB.querySettings >>= maybe (return ()) action

-- | creates a new WS client application that checks plant moisture on a regular
-- basis.
client :: WS.ClientApp ()
client conn = do
    forkIO . forever $ do
        msg <- WS.receiveData conn
        liftIO . when ("triggerPump" `T.isInfixOf` msg) $ manualPump conn

    liftIO $ checkMoisture conn
