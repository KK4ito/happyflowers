{-# LANGUAGE OverloadedStrings, CPP #-}

module HappyFlowers.Hardware.Client
    (
      -- * Operations
      client
    ) where

import           Control.Concurrent                  (forkIO)
import           Control.Monad                       (forever, when)
import           Control.Monad.Trans                 (liftIO)
import           Data.Text                           (isInfixOf)
import qualified Network.WebSockets                  as WS

import           HappyFlowers.Hardware.Communication (checkMoisture, manualPump)

-- | creates a new WS client application that checks plant moisture on a regular
-- basis.
client :: WS.ClientApp ()
client conn = do
    forkIO . forever $ do
        msg <- WS.receiveData conn
        liftIO . when ("triggerPump" `isInfixOf` msg) $ manualPump conn

#ifdef Development
    liftIO $ checkMoisture conn
#else
    forever $ checkMoisture conn
#endif
