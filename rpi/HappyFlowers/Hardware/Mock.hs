module HappyFlowers.Hardware.Mock
    ( readMoisture
    , readTemperature
    , triggerPump
    ) where

import Control.Concurrent (threadDelay)

-- | reads data about moisture from the chirp sensor.
readMoisture :: IO Int
readMoisture = putStrLn "sensor on" >> putStrLn "sensor off" >> return 80

-- | reads data about temperature from the chirp sensor.
readTemperature :: IO Int
readTemperature = putStrLn "sensor on" >> putStrLn "sensor off" >> return 25

-- | triggers the USB water pump.
triggerPump :: IO ()
triggerPump = putStrLn "pump on" >> threadDelay 5000000 >> putStrLn "pump off" >> threadDelay 5000000
