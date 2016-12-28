{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.WS.Application
Description : WebSockets server implementation for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.WS.Application
    (
      -- * Operations
      wsApp
    , callback
    ) where

import           Control.Concurrent            (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception             (finally)
import           Control.Monad                 (forever)
import           Data.Monoid                   (mappend)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

import qualified HappyFlowers.DB               as DB
import           HappyFlowers.Hardware.Process (checkMoisture, activatePump, manualPump)
import           HappyFlowers.Type             (BusyState(..), WSEventKind(..), Command(..), Client, ServerState)
import           HappyFlowers.WS.Communication (notify, notifyAll)

-- | checks if a client with the given ID already exists.
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- | adds a new 'Client' to the current 'ServerState' using the currently
-- available 'Id'.
addClient :: Client -> ServerState -> ServerState
addClient client clients = (client : clients)

-- | removes a 'Client' from the current 'ServerState'.
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- | retrieves a message from a connected 'Client' and broadcasts it to all
-- other clients.
listen :: WS.Connection -> MVar ServerState -> MVar BusyState -> IO ()
listen conn state busy = forever $ do
    msg <- WS.receiveData conn
    case msg of
        _ | (T.pack $ show TriggerPump) `T.isInfixOf` msg -> do
            settings <- DB.querySettings
            case settings of
                Just settings' -> manualPump settings' busy $ callback busy state
                Nothing        -> return ()
          | (T.pack $ show SettingsChanged) `T.isInfixOf` msg -> do
              -- TODO: broadcast
              return ()
          | otherwise -> return ()

-- | removes a 'Client' from the current ServerState and broadcasts it to all
-- other clients.
disconnect :: Client -> MVar ServerState -> IO [Client]
disconnect client state = do
    modifyMVar state $ \s -> do
        let s' = removeClient client s
        return (s', s')

-- TODO: document
callback :: MVar BusyState -> MVar ServerState -> Command -> IO ()
callback busy state cmd = do
    case cmd of
        SaveEvent kind             -> do
            DB.addEvent kind
            e <- DB.queryLatestEvent
            notifyAll state EventReceived e
        SaveMeasurement kind value -> do
            DB.addMeasurement kind value
            m <- DB.queryLatestMeasurement kind
            notifyAll state MeasurementReceived m
        UpdateBusy busyState       -> do
            b <- modifyMVar busy $ \s -> return (busyState, busyState)
            notifyAll state BusyChanged $ b == Busy
        PumpRequired               -> do
            settings <- DB.querySettings
            case settings of
                Just settings' -> activatePump settings' busy $ callback busy state
                Nothing        -> return ()
        CheckRequired              -> do
            settings <- DB.querySettings
            case settings of
                Just settings' -> checkMoisture settings' busy $ callback busy state
                Nothing        -> return ()

-- | starts a new WebSockets server instance. New connections recieve all
-- broadcasts.
server :: MVar ServerState -> MVar BusyState -> WS.ServerApp
server state busy pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    msg <- WS.receiveData conn
    clients <- readMVar state

    let client = (msg, conn)

    case msg of
        _ | clientExists client clients -> WS.sendTextData conn ("User already exists" :: Text)
          | otherwise                   -> flip finally (disconnect client state) $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    h <- DB.queryHistory
                    notify conn HistoryReceived h
                    return s'

                b <- readMVar busy
                notify conn BusyChanged $ b == Busy

                listen conn state busy

-- | sets up a WebSockets server listening on a given port.
wsApp :: Int -- Port
      -> MVar ServerState
      -> MVar BusyState
      -> IO ()
wsApp port state busy = WS.runServer "0.0.0.0" port $ server state busy
