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
    ) where

import Data.Aeson                 (ToJSON, encode, object, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text                  (Text, pack)
import Network.WebSockets         (Connection, sendTextData)

import HappyFlowers.Type          (WSEventKind)

-- | sends WS notifications to all connected clients about measurements or
-- events.
notify :: ToJSON a => Connection -> WSEventKind -> a -> IO ()
notify conn kind payload = sendTextData conn $ createMessage kind payload

-- | creates a WS notification body containing a JSON-encoded kind and payload.
createMessage :: ToJSON a => WSEventKind -> a -> Text
createMessage kind payload = pack . unpack . encode $ object
    [ "kind" .= (show kind)
    , "payload" .= payload
    ]
