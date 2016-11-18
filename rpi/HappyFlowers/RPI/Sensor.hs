{-|
Module      : HappyFlowers.RPI.Sensor
Description : Read sensor data from the RPi device
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to read the sensor data from the
RPi's sensors.
-}
module HappyFlowers.RPI.Sensor (
  -- * Configuration
  address,
  -- * Operations
  readData
  ) where

import System.RaspberryPi.GPIO
import qualified Data.ByteString.Char8 as C

-- | 'address' determines the address of the port that is used to read data.
address :: Address
address = 0x20

-- | The 'readData' function reads data from the chirp sensor through the
-- 'address'. It relies on GPIO and I2C.
readData :: IO ()
readData = withGPIO . withI2C $ do
  d <- readI2C address 0
  putStrLn $ C.unpack d
