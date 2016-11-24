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
    Client(..)
  , Id(..)
  , ServerState(..)
    -- * Operations
  , newServerState
  , addClient
  , removeClient
  , broadcast
  , server
  , talk
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

-- | 'Client' is a type alias that connects a unique ID with a WebSockets
-- connection.
type Client = (Id, WS.Connection)

-- | 'Id' is a type alias that represents the unique ID on the WebSockets
-- server.
type Id = Int

-- | 'ServerState' keeps track of the currently available Id and the list of
-- connected clients.
type ServerState = (Id, [Client])

-- | The 'newServerState' function creates a new 'ServerState' instance with the
-- default values.
newServerState :: ServerState
newServerState = (0, [])

-- | The 'addClient' function adds a new 'Client' to the current 'ServerState'.
-- The currently available Id is incremented so that it can be claimed by the
-- next client.
addClient :: Client -> ServerState -> ServerState
addClient client (id, clients) = (id + 1, client : clients)

-- | The 'removeClient' function removes a client from the current
-- 'ServerState'. The currently available Id is unaffected by this change.
removeClient :: Client -> ServerState -> ServerState
removeClient client (id, clients) = (id, filter ((/= fst client) . fst) clients)

-- | The 'broadcast' function is used to send a message to all connected
-- clients.
broadcast :: Text -> ServerState -> IO ()
broadcast message (_, clients) = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- | The 'talk' function is used to retrieve a message from a client and
-- 'broadcast' it to all clients.
talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state _ = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg

-- | The 'disconnect' function is used to remove a client from the current
-- 'ServerState' instance. This change is broadcast to all other clients.
disconnect :: Client -> MVar ServerState -> IO [Client]
disconnect client state = do
  modifyMVar state $ \s -> do
    let s' = removeClient client s
    broadcast (T.pack (show . fst $ client) `mappend` " disconnected") s'
    return (s', snd s')

-- | The 'server' function starts a new WebSockets server instance that accepts
-- new connections. All new connections are added to the 'ServerState' and
-- receive all broadcasts.
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

-- | The 'wsApp' function sets up a WebSockets server listening on a given port.
wsApp :: Int -- ^ Port
      -> IO ()
wsApp port = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" port $ server state
