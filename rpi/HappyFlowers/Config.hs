{-|
Module      : HappyFlowers.Config
Description : Configuration parser for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to parse configuration data for
the happy flowers project.
-}
module HappyFlowers.Config
  (
    -- * Configuration
    configFile
    -- * Operations
  , readConfig
  , getConfig
    -- * Helpers
  , splitToTuple
  ) where

import qualified Data.Text as T

-- | The 'ConfigEntry' type is a shorthand for the (key, value) representation
-- of a config entry.
type ConfigEntry = (String, String)

-- | 'configFile' determines the location of the configuration file that is read
-- when parsing the application configuration.
configFile :: FilePath
configFile = "rpi.cfg"

-- | The 'readConfig' function converts the value of the config file to a list
-- of [(key, value)] format.
readConfig :: FilePath -> IO [ConfigEntry]
readConfig file = do
  val <- readFile file
  return . fmap splitToTuple $ lines val

-- | The 'getConfig' function reads the list of entries parsed from the config
-- file and filters it in order to obtain the desired config. It matches entries
-- based on the name of the entry, which is the first element of every entry
-- tuple and returns the last element of the matching tuple.
getConfig :: String -- ^ Config entry name
          -> IO String
getConfig field = do
  config <- readConfig configFile
  let entry = filter ((== field) . fst) config
  return . snd . head $ entry

-- | The 'splitToTuple' takes a String, splits it at every '=' character and
-- converts the result to a ConfigEntry tuple.
splitToTuple :: String -- ^ Key-value pair
             -> ConfigEntry
splitToTuple t = do
  let (key:val:_) = ((T.split (== '=')) . T.pack) t
  (T.unpack key, T.unpack val)
