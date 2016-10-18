module REST.Config (
  getPassword
) where

getPassword :: IO String
getPassword = do
  val <- readFile "rpi.cfg"
  return $ filter (/= '\n') val
