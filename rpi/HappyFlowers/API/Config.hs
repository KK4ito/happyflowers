{-|
Module      : HappyFlowers.API.Config
Description : Configuration parser for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to parse configuration data for
the happy flowers project.
-}
module HappyFlowers.API.Config (
  -- * Operations
  getPassword
  ) where

-- | The function 'getPassword' reads the configuration file named 'rpi.cfg'
-- stored at the root directory of the project. The file is expected to only
-- contain the password.
getPassword :: IO String
getPassword = do
  val <- readFile "rpi.cfg"
  return $ filter (/= '\n') val
