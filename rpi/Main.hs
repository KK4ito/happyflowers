{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import Network.HTTP.Types.Status (notImplemented501, unauthorized401)
import System.Directory
import Web.Scotty

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
  eventType :: String,
  eventTimestamp :: Int
} deriving (Show, Generic)

instance ToJSON Event

data Measurement = Measurement {
  measurementValue :: Int,
  measurementTimestamp :: Int
} deriving (Show, Generic)

instance ToJSON Measurement

data History = Hisotry {
  events :: [Event],
  measurements :: [Measurement]
} deriving (Show, Generic)

instance ToJSON History

configFile :: FilePath
configFile = "rpi.cfg"

readPwd :: FilePath -> IO String
readPwd file = do
  exists <- doesFileExist file
  if not exists
    then error "Config file does not exist"
    else do
      content <- readFile file
      let pwd = if null content then error "Config file is empty" else content
      return pwd

main = scotty 3000 $ do
  get "/settings" $ do
    conn <- liftIO (open "happyflowers.db")
    rows <- liftIO (query_ conn "SELECT * FROM settings" :: IO [Settings])
    json $ head rows
    liftIO (close conn)
  put "/settings" $ do
    status notImplemented501
  get "/history" $ do
    status notImplemented501
  post "/auth" $ do
    -- TODO: read config file, check if config password matches password from request
    -- body, give ok or unauthorized
    status unauthorized401
