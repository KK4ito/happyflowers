{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.API.Server
Description : Scotty web server implementation for the happy flowers project
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

An implementation of the Scotty web framework offering a RESTful API and serving
static files.
-}
module HappyFlowers.API.Server (
  -- * Middlewares
  pathConversion,
  -- * Operations
  startServer
  ) where

import Data.Text (Text)
import HappyFlowers.API.Routes
import Network.HTTP.Types as H
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Rewrite
import Network.Wai.Middleware.Static
import Web.Scotty

-- | The 'pathConversion' function rewrites URLS based on certain criteria.
-- Requests to endpoints starting with api are simply passed through as-is,
-- while all other requests are redirected to the root route.
pathConversion :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
pathConversion (pieces, queries) _ = piecesConvert pieces queries
  where
    piecesConvert :: [Text] -> H.Query -> PathsAndQueries
    piecesConvert ["api", method, trail] qs = (["api", method, trail], qs)
    piecesConvert ps qs = (["/"], qs)

-- | The 'startServer' function sets up a local Scotty server listening on port
-- 5000. It contains several middlewares and reacts to a set of routes.
startServer = scotty 5000 $ do

  -- Set up a piece of middleware that is used during development. It allows
  -- cross-origin requests between the webpack development server and the API.

  middleware $ cors $ const $ Just simpleCorsResourcePolicy {
    corsOrigins = Just (["http://localhost:3000"], False),
    corsMethods = ["GET", "PUT", "POST", "OPTIONS"]
  }

  -- Set up a piece of middleware that allows serving static files from the
  -- compiled web front end directory.

  middleware $ staticPolicy (addBase "../web/build/")

  -- Set up a piece of middleware that allows request rewriting. It is used to
  -- route all non-API requests to the root route, where the web front end
  -- handles routing.

  middleware $ rewritePureWithQueries pathConversion

  -- Include all routes.

  getSettings >> putSettings >> getHistory >> postAuth >> getRoot
