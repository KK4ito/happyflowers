{-|
Module      : HappyFlowers.API.Application
Description : Scotty web server implementation for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental
-}

module HappyFlowers.API.Application
    (
      -- * Operations
      apiApp
    ) where

import Web.Scotty                  (middleware, scotty)

import HappyFlowers.API.Middleware
import HappyFlowers.API.Route

-- | sets up a Scotty server listening on a given port. Contains several
-- middlewares and reacts to a set of routes.
apiApp :: Int -- ^ Port
       -> IO ()
apiApp port = scotty port $ do
    middleware staticMiddleware
    middleware rewriteMiddleware
    getSettings >> putSettings >> getHistory >> postAuth >> getRoot
