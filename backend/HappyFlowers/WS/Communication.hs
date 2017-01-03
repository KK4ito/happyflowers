{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.WS.Communication
Description : Communicate between WebSockets clients
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.WS.Communication
    (
      -- * Operations
      notify
    , notifyAll
    ) where

import Control.Concurrent         (MVar, readMVar)
import Control.Monad              (forM_)
import Data.Aeson                 (ToJSON, encode, object, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text                  (Text, pack)
import Network.WebSockets         (Connection, sendTextData)

import HappyFlowers.Type          (WSEventKind, ServerState)

-- | sends WS notifications to all connected clients.
notifyAll :: ToJSON a => MVar ServerState -> WSEventKind -> a -> IO ()
notifyAll state kind payload = do
    clients <- readMVar state
    forM_ clients $ \(_, conn) -> notify conn kind payload

-- | sends WS notifications to a specific client.
notify :: ToJSON a => Connection -> WSEventKind -> a -> IO ()
notify conn kind payload = sendTextData conn $ createMessage kind payload

-- | creates a WS notification body containing a JSON-encoded kind and payload.
createMessage :: ToJSON a => WSEventKind -> a -> Text
createMessage kind payload = pack . unpack . encode $ object
    [ "kind" .= (show kind)
    , "payload" .= payload
    ]
