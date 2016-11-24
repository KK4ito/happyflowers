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

module HappyFlowers.API.Application
    (
      -- * Operations
      apiApp
    ) where

import qualified HappyFlowers.API.Middleware as M
import qualified HappyFlowers.API.Route      as R

import           Web.Scotty                   (middleware, scotty)

-- | The 'apiApp' function sets up a Scotty server listening on a given port. It
-- contains several middlewares and reacts to a set of routes.
apiApp :: Int -- ^ Port
       -> IO ()
apiApp port = scotty port $ do
    middleware M.staticMiddleware
    middleware M.rewriteMiddleware
    R.getSettings >> R.putSettings >> R.getHistory >> R.postAuth >> R.getRoot
