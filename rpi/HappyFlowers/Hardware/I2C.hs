{-|
Module      : HappyFlowers.Hardware.I2C
Description : Communicate through I2C
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : stable
-}
module HappyFlowers.Hardware.I2C
    (
      -- * Operations
      read
    ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C
import           Data.Char               (ord)
import           Data.Word               (Word8)
import           Prelude                 hiding (read)
import           System.RaspberryPi.GPIO (Address, withI2C, readI2C, writeI2C)

-- | determines the address of the port that is used to read data.
address :: Address
address = 0x20

-- | reads the value from a given I2C register.
read :: Word8    -- Number of the register
     -> IO Int -- Parsed value
read reg = withI2C $ do
    writeI2C address (BS.singleton reg)
    val <- readI2C address 2
    return $ parse val

-- | parses a pair of bytes read through I2C.
parse :: BS.ByteString -- I2C byte string pair
      -> Int           -- Parsed value
parse s = do
    let [high, low] = C.unpack s
    256 * ord high + ord low
