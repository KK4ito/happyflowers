{-|
Module      : HappyFlowers.Hardware.GPIO
Description : Communicate through GPIO
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental
-}
module HappyFlowers.Hardware.GPIO
    (
      -- * Operations
      activatePin
    , deactivatePin
    ) where

import System.RaspberryPi.GPIO (withGPIO, setPinFunction, writePin, Pin(..), PinMode(..))

-- | determines the number of the pin that is used to communicate through GPIO.
pin :: Pin
pin = Pin07

-- | activates the pin.
activatePin :: IO ()
activatePin num = withGPIO $ do
    setPinFunction pin Output
    writePin pin True

-- | deactivates the pin.
deactivatePin :: IO ()
deactivatePin num = withGPIO $ do
    setPinFunction pin Output
    writePin pin False
