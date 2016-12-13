module HappyFlowers.Hardware.Mock
    ( readMoisture
    , readTemperature
    , triggerPump
    ) where

import Control.Concurrent (threadDelay)

-- | determines the length of the delay after the pump has been activated.
pumpDelay :: Int
pumpDelay = 5000000

-- | reads data about moisture from the chirp sensor.
readMoisture :: IO Int
readMoisture = putStrLn "sensor on" >> putStrLn "sensor off" >> return 80

-- | reads data about temperature from the chirp sensor.
readTemperature :: IO Int
readTemperature = putStrLn "sensor on" >> putStrLn "sensor off" >> return 25

-- | triggers the USB water pump.
triggerPump :: IO ()
triggerPump = putStrLn "pump on" >> threadDelay pumpDelay >> putStrLn "pump off" >> threadDelay pumpDelay
