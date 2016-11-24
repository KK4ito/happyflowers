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
    , getConfig
      -- * Helpers
    , splitToTuple
    ) where

import           Control.Applicative ((<$>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TI

-- | The 'ConfigEntry' type is a shorthand for the (key, value) representation
-- of a config entry.
type ConfigEntry = (T.Text, T.Text)

-- | determines the location of the configuration file.
configFile :: FilePath
configFile = "rpi.cfg"

-- | reads the config file and filters it in order to obtain the desired config
-- entry.
getConfig :: T.Text -- ^ Config entry name
          -> IO T.Text
getConfig field = do
    val <- TI.readFile configFile
    let config = splitToTuple <$> T.lines val
    return $ findEntry config field

-- TODO: document
findEntry :: [ConfigEntry] -- ^ Full configuration
          -> T.Text -- ^ Config entry name
          -> T.Text -- ^ Config entry value
findEntry config name = snd . head $ filter ((== name) . fst) config

-- | splits a string into a 'ConfigEntry' tuple.
splitToTuple :: T.Text -- ^ Key-value pair
             -> ConfigEntry
splitToTuple line = (key, val)
    where
        (key:val:_) = T.split (== '=') line
