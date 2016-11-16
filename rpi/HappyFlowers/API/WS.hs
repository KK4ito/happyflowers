{-# LANGUAGE OverloadedStrings #-}

module HappyFlowers.API.WS (
  -- * Types
  Client(..),
  Id(..),
  ServerState(..),
  -- * Operations
  newServerState,
  numClients,
  clientExists,
  addClient,
  removeClient,
  broadcast,
  application,
  talk
  ) where

import           Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception (finally)
import           Control.Monad (forM_, forever)
import           Data.Char (isPunctuation, isSpace)
import           Data.Monoid (mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- todo: document
type Client = (Int, WS.Connection)

-- todo: document
type Id = Int

-- todo: document
type ServerState = (Id, [Client])

-- todo: document
newServerState :: ServerState
newServerState = (0, [])

-- todo: document
numClients :: ServerState -> Int
numClients (_, clients) = length clients

-- todo: document
clientExists :: Client -> ServerState -> Bool
clientExists client (_, clients) = any ((== fst client) . fst) clients

-- todo: document
addClient :: Client -> ServerState -> ServerState
addClient client (id, clients) = (id + 1, client : clients)

-- todo: document
removeClient :: Client -> ServerState -> ServerState
removeClient client (id, clients) = (id, filter ((/= fst client) . fst) clients)

-- todo: document
broadcast :: Text -> ServerState -> IO ()
broadcast message (_, clients) = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- todo: document
talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state _ = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg

-- todo: document
disconnect :: Client -> MVar ServerState -> IO [Client]
disconnect client state = do
  modifyMVar state $ \s -> do
    let s' = removeClient client s
    broadcast (T.pack ((show . fst) client) `mappend` " disconnected") s'
    return (s', snd s')

-- todo: document
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  
  (id, _) <- readMVar state

  let client = (id, conn)

  flip finally (disconnect client state) $ do
    modifyMVar_ state $ \s -> do
      let s' = addClient client s
      broadcast (T.pack ((show . fst) client) `mappend` " joined") s'
      return s'
    talk conn state client
