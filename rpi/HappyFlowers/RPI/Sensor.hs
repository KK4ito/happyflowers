{-|
Module      : HappyFlowers.RPI.Sensor
Description : Read sensor data
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to read the sensor data from the chirp sensor.
-}
module HappyFlowers.RPI.Sensor (
  -- * Configuration
  address,
  -- * Operations
  readData
  ) where

import System.RaspberryPi.GPIO

address :: Address
address = 0x20

readData :: IO ()
readData = withGPIO . withI2C $ do
  data <- readI2C address 0
  putStrLn data
