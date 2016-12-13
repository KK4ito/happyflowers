{-# LANGUAGE OverloadedStrings #-}

-- | TODO document
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

-- | TODO document
createMessage :: ToJSON a => WSEventKind -> a -> Text
createMessage kind payload = pack . unpack . encode $ object
    [ "kind" .= (show kind)
    , "payload" .= payload
    ]
