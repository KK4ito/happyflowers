{-|
Module      : HappyFlowers.API.Application
Description : Scotty web server implementation for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

An implementation of the Scotty web framework offering a RESTful API and serving
static files.
-}

module HappyFlowers.API.Application (
  -- * Operations
  apiApp
  ) where

import HappyFlowers.API.Middlewares
import HappyFlowers.API.Routes
import Web.Scotty

-- | The 'apiApp' function sets up a Scotty server listening on a given port. It
-- contains several middlewares and reacts to a set of routes.
apiApp :: Int -> IO ()
apiApp port = scotty port $ do
  middleware corsMiddleware
  middleware staticMiddleware
  middleware rewriteMiddleware
  getSettings >> putSettings >> getHistory >> postAuth >> getRoot
