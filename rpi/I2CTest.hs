{-|
Module      : I2CTest
Description : Test I2C functionality
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module I2CTest
    (
      -- * Operations
      main
    ) where

import           Data.Char                 (ord)
import           Control.Monad             (forever)
import           Control.Concurrent        (threadDelay)
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString           as BS

import qualified HappyFlowers.Hardware.I2C as I2C

-- | reads moisture and temperature data through I2C.
main :: IO ()
main = withGPIO . withI2C $ do
    putStrLn "Starting Chirp"
    forever $ do
        I2C.read 0 >>= putStrLn . show
        I2C.read 5 >>= putStrLn . show
        threadDelay 3000000
