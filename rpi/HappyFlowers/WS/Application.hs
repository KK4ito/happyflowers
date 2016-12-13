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
      -- * Types
      Client(..)
    , ServerState(..)
      -- * Operations
    , newServerState
    , wsApp
    ) where

import           Control.Concurrent            (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception             (finally)
import           Control.Monad                 (forM_, forever)
import           Data.Monoid                   (mappend)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

import qualified HappyFlowers.DB               as DB
import           HappyFlowers.Type             (BusyState(..))
import           HappyFlowers.WS.Communication (notify)

-- | 'Client' is a type alias that connects a unique ID with a WebSockets
-- connection.
type Client = (T.Text, WS.Connection)

-- | keeps track of the currently available Id and the list of connected
-- clients.
type ServerState = [Client]

-- | creates a new 'ServerState' instance with the default values.
newServerState :: ServerState
newServerState = []

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

-- | sends a message to all connected clients.
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- | retrieves a message from a connected 'Client' and broadcasts it to all
-- other clients.
talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state _ = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast msg

-- | removes a 'Client' from the current ServerState and broadcasts it to all
-- other clients.
disconnect :: Client -> MVar ServerState -> IO [Client]
disconnect client state = do
    modifyMVar state $ \s -> do
        let s' = removeClient client s
        return (s', s')

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
                    notify conn "historyReceived" h
                    return s'

                b <- readMVar busy
                notify conn "busyChanged" (b == Busy :: Bool)

                talk conn state client

-- | sets up a WebSockets server listening on a given port.
wsApp :: Int -- Port
      -> MVar BusyState
      -> IO ()
wsApp port busy = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" port $ server state busy
