{-|
Module      : HappyFlowers.Config
Description : Configuration parser for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Config
    (
      -- * Operations
      getConfig
    ) where

import           Control.Applicative ((<$>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TI

-- | A config entry is the (key, value) representation of a configuration
-- option.
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

-- | finds a config entry's value based on its name.
findEntry :: [ConfigEntry] -- ^ Full configuration
          -> T.Text        -- ^ Config entry name
          -> T.Text        -- ^ Config entry value
findEntry config name = snd . head $ filter ((== name) . fst) config

-- | splits a string into a ConfigEntry tuple.
splitToTuple :: T.Text -- ^ Key-value pair
             -> ConfigEntry
splitToTuple line = do
    let [key, val] = take 2 $ T.split (== '=') line
    (key, val)
