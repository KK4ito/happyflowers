module HappyFlowers.Hardware.Device
    ( readMoisture
    , readTemperature
    , triggerPump
    ) where

import           Control.Concurrent         (threadDelay)

import qualified HappyFlowers.Hardware.I2C  as I2C
import qualified HappyFlowers.Hardware.GPIO as GPIO

-- | reads data about moisture from the chirp sensor.
readMoisture :: IO Int
readMoisture = do
    val <- I2C.read 0

    let numeral = fromIntegral (val - 100) :: Rational
    let relative = round (numeral / 700.0 * 100.0) :: Int

    case relative of
        _ | relative < 0   -> return 0
          | relative > 100 -> return 100
          | otherwise      -> return relative

-- | reads data about temperature from the chirp sensor.
readTemperature :: IO Int
readTemperature = do
    val <- I2C.read 5
    let numeral = fromIntegral val :: Rational
    return (round (numeral / 10.0) :: Int)

-- | triggers the USB water pump.
triggerPump :: IO ()
triggerPump = do
    GPIO.activatePin
    threadDelay 5000000
    GPIO.deactivatePin
    threadDelay 5000000
