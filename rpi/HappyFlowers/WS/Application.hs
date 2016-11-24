{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.WS.Application
Description : WebSockets server implementation for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

An implementation of a WebSockets server that simply broadcasts messages
between clients.
-}
module HappyFlowers.WS.Application
    (
      -- * Types
      Id(..)
    , Client(..)
    , ServerState(..)
      -- * Operations
    , newServerState
    , wsApp
    ) where

import           Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forM_, forever)
import           Data.Char          (isPunctuation, isSpace)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

-- | 'Id' is a type alias that represents the unique ID on the WebSockets
-- server.
type Id = Int

-- | 'Client' is a type alias that connects a unique ID with a WebSockets
-- connection.
type Client = (Id, WS.Connection)

-- | keeps track of the currently available Id and the list of connected
-- clients.
type ServerState = (Id, [Client])

-- | creates a new 'ServerState' instance with the default values.
newServerState :: ServerState
newServerState = (0, [])

-- | adds a new 'Client' to the current 'ServerState' using the currently
-- available 'Id'.
addClient :: Client -> ServerState -> ServerState
addClient client (id, clients) = (id + 1, client : clients)

-- | removes a 'Client' from the current 'ServerState'.
removeClient :: Client -> ServerState -> ServerState
removeClient client (id, clients) = (id, filter ((/= fst client) . fst) clients)

-- | sends a message to all connected clients.
broadcast :: Text -> ServerState -> IO ()
broadcast message (_, clients) = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

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
        broadcast (T.pack (show . fst $ client) `mappend` " disconnected") s'
        return (s', snd s')

-- | starts a new WebSockets server instance. New connections recieve all
-- broadcasts.
server :: MVar ServerState -> WS.ServerApp
server state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    (id, _) <- readMVar state

    let client = (id, conn)

    flip finally (disconnect client state) $ do
        modifyMVar_ state $ \s -> do
            let s' = addClient client s
            broadcast (T.pack (show . fst $ client) `mappend` " joined") s'
            return s'
        talk conn state client

-- | sets up a WebSockets server listening on a given port.
wsApp :: Int -- ^ Port
      -> IO ()
wsApp port = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" port $ server state
