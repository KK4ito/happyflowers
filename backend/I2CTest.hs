{-|
Module      : Main
Description : Test I2C functionality
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module Main
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
main = do
    putStrLn "Starting Chirp"
    forever $ do
        putStr "Moisture:\t" >> I2C.read 0 >>= print
        putStr "Temperature:\t" >> I2C.read 5 >>= print
        threadDelay 3000000
