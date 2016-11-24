-- TODO: document
module HappyFlowers.Hardware.Application
    (
      -- * Operations
      client
    , hwApp
    ) where

import           Control.Monad.Trans                 (liftIO)
import           Network.Socket                      (withSocketsDo)
import qualified Network.WebSockets                  as WS

import qualified HappyFlowers.Hardware.Communication as C

-- TODO: document
client :: WS.ClientApp ()
client conn = do
    liftIO $ C.checkMoisture conn

-- TODO: document
hwApp :: Int -- ^ Port
      -> IO ()
hwApp port = withSocketsDo $ WS.runClient "127.0.0.1" port "/" client
