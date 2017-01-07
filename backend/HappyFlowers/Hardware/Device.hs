{-|
Module      : HappyFlowers.Hardware.Device
Description : Communication with the sensor and pump
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Hardware.Device
    (
      -- * Operations
      readMoisture
    , readTemperature
    , triggerPump
    ) where

import           Control.Concurrent         (threadDelay)

import qualified HappyFlowers.Hardware.I2C  as I2C
import qualified HappyFlowers.Hardware.GPIO as GPIO

-- | determines the length of the delay after the pump has been activated.
pumpDelay :: Int
pumpDelay = 4000000

-- | reads data about moisture from the chirp sensor.
readMoisture :: IO Int
readMoisture = do
    val <- I2C.read 0
    if val > 800 then readMoisture else return $ convertMoisture val

-- | converts I2C moisture to a relative value between 0 and 100.
convertMoisture :: Int -> Int
convertMoisture val = do
    let numeral = fromIntegral (val - 100) :: Rational
    let relative = round (numeral / 700.0 * 100.0) :: Int
    min 100 $ max 0 relative

-- | reads data about temperature from the chirp sensor.
readTemperature :: IO Int
readTemperature = do
    val <- I2C.read 5
    if val > 50 then readTemperature else return $ convertTemperature val

-- | converts I2C temperature to degrees celcius.
convertTemperature :: Int -> Int
convertTemperature val = do
    let numeral = fromIntegral val :: Rational
    round (numeral / 10.0) :: Int

-- | triggers the USB water pump.
triggerPump :: IO ()
triggerPump = do
    GPIO.activatePin
    threadDelay pumpDelay
    GPIO.deactivatePin
    threadDelay pumpDelay
