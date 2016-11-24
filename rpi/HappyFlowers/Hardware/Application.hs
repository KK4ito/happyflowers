-- TODO: document
module HappyFlowers.Hardware.Application (
  -- * Operations
  client,
  hwApp
  ) where

import qualified HappyFlowers.Hardware.Communication as C

import           Control.Monad.Trans                 (liftIO)
import           Network.Socket                      (withSocketsDo)
import qualified Network.WebSockets                  as WS

-- TODO: document
client :: WS.ClientApp ()
client conn = do
  liftIO $ C.checkMoisture conn

-- TODO: document
hwApp :: Int -> IO ()
hwApp port = withSocketsDo $ WS.runClient "localhost" port "/" client
