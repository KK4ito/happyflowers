{-# LANGUAGE OverloadedStrings #-}

module HappyFlowers.WS.Communication
    (
      -- * Operations
      notify
    ) where

import Data.Aeson                 (ToJSON, encode, object, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text                  (Text, pack)
import Network.WebSockets         (Connection, sendTextData)

-- | sends WS notifications to all connected clients about measurements or
-- events.
notify :: ToJSON a
       => Connection -- Connection to send data to
       -> Text       -- WS event type
       -> a          -- Payload
       -> IO ()
notify conn kind payload = sendTextData conn $ createMessage kind payload

createMessage :: ToJSON a
              => Text -- WS event type
              -> a    -- Payload
              -> Text
createMessage kind payload = pack . unpack . encode $ object
    [ "kind" .= kind
    , "payload" .= payload
    ]
