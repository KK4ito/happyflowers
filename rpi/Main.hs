{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Exception
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Char8 as C
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import Jose.Jws
import Jose.Jwa
import Jose.Jwt
import Network.HTTP.Types.Status (ok200, internalServerError500, unauthorized401)
import Network.Wai.Middleware.Cors
import System.Directory
import Web.Scotty
import Web.Scotty.Cookie

data Settings = Settings {
  name :: String,
  upper :: Int,
  lower :: Int,
  interval :: Int
} deriving (Show, Generic)

instance ToJSON Settings
instance FromRow Settings where
  fromRow = Settings <$> field <*> field <*> field <*> field

data Event = Event {
  eventId :: Int,
  eventType :: String,
  eventTimestamp :: String
} deriving (Show, Generic)

instance ToJSON Event
instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field

data Measurement = Measurement {
  measurementId :: Int,
  measurementValue :: Int,
  measurementTimestamp :: String
} deriving (Show, Generic)

instance ToJSON Measurement
instance FromRow Measurement where
  fromRow = Measurement <$> field <*> field <*> field

data History = History {
  events :: [Event],
  measurements :: [Measurement]
} deriving (Show, Generic)

instance ToJSON History

getPassword :: IO String
getPassword = do
  val <- readFile "rpi.cfg"
  return $ filter (/= '\n') val

main = scotty 5000 $ do

  middleware $ cors $ const $ Just simpleCorsResourcePolicy {
    corsMethods = ["GET", "PUT", "POST", "OPTIONS"]
  }

  get "/settings" $ do
    conn <- liftIO (open "happyflowers.db")
    rows <- liftIO (try (query_ conn "SELECT * FROM settings") :: IO (Either SQLError [Settings]))
    case rows of
      Left e -> status internalServerError500
      Right r -> if length r > 0 then json (head r) else status internalServerError500
    liftIO (close conn)

  put "/settings" $ do
    token <- (param "token") :: ActionM String
    let jwt = hmacDecode "hppyflwrs" $ C.pack token
    case jwt of
      Right _ -> do
        name <- (param "name") :: ActionM String
        upper <- (param "upper") :: ActionM Int
        lower <- (param "lower") :: ActionM Int
        interval <- (param "interval" ) :: ActionM Int
        conn <- liftIO (open "happyflowers.db")
        liftIO (execute conn "UPDATE settings SET name = ?, upper = ?, lower = ?, interval = ?" (name, upper, lower, interval))
        json Settings { name = name, upper = upper, lower = lower, interval = interval }
        liftIO (close conn)
      Left e -> status unauthorized401

  get "/history" $ do
    conn <- liftIO (open "happyflowers.db")
    events <- liftIO (query_ conn "SELECT * FROM events WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC" :: IO [Event])
    measurements <- liftIO (query_ conn "SELECT * FROM measurements WHERE date(timestamp) >= date('now', '-14 days') ORDER BY timestamp ASC" :: IO [Measurement])
    json History { events = events, measurements = measurements }
    liftIO (close conn)

  post "/auth" $ do
    pw <- (param "password") :: ActionM String
    syspw <- liftIO getPassword
    if pw == syspw
      then do
        let jwt = hmacEncode HS384 "hppyflwrs" "hello"
        case jwt of
          Right j -> do
            json j
      else do
        status unauthorized401
