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
module HappyFlowers.Config (
  -- * Operations
  getConfig
  ) where

import qualified Data.Text as T

-- | The function 'getConfig' reads the configuration file named 'rpi.cfg'
-- stored at the root directory of the project. The Text passed as the
-- argument determines the entry that is read from the config.
getConfig :: T.Text -> IO String
getConfig field = do
  val <- readFile "rpi.cfg"

  -- Convert the value of the file to a Text, split it into lines and then split
  -- every line at the `=` character.
  let config = map (T.split (== '=')) $ (T.lines . T.pack) val

  -- Config entries are parsed as [[key, value]]. The list of entries is
  -- filtered in order to retrieve the entry matching the parameter. Then the
  -- first match's value field is unpacked to a string and returned.
  let entry = head $ filter (\(x:xs) -> x == field) config
  let value = last entry
  return $ T.unpack value
