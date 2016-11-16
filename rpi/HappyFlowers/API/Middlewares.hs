{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HappyFlowers.API.Middleware
Description : Modify requests to the server
Copyright   : (c) Sacha Schmid, 2016
                  Rinesch Murugathas, 2016
License     : GPL-3
Maintainer  : schmid.sacha@gmail.com
Stability   : experimental

The functions included in this module are used to modify incoming requests to
the server to extend the response with data or facilitate React routing.
-}
module HappyFlowers.API.Middlewares (
  -- * Operations
  corsMiddleware,
  staticMiddleware,
  rewriteMiddleware,
  pathConversion
  ) where

import Data.Text (Text)
import Network.HTTP.Types as H
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Rewrite
import Network.Wai.Middleware.Static

-- | The 'corsMiddleware' function sets up a piece of middleware that is used
-- during development. It allows cross-origin requests between the webpack
-- development server and the API.
corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just simpleCorsResourcePolicy
  { corsOrigins = Just (["http://localhost:3000", "http://localhost:5000"], False)
  , corsRequestHeaders = ["Content-Type"]
  , corsMethods = ["GET", "PUT", "POST", "OPTIONS"]
  }

-- | The 'staticMiddleware' function sets up a piece of middleware that allows
-- serving static files from the compiled web front end directory.
staticMiddleware :: Middleware
staticMiddleware = staticPolicy (addBase "../web/build/")

-- | The 'rewriteMiddleware' function sets up a piece of middleware that allows
-- request rewriting. It is used to route all non-API requests to the root
-- route, where the web front end handles routing.
rewriteMiddleware :: Middleware
rewriteMiddleware = rewritePureWithQueries pathConversion

-- | The 'pathConversion' function rewrites URLS based on certain criteria.
-- Requests to endpoints starting with api are simply passed through as-is,
-- while all other requests are redirected to the root route.
pathConversion :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
pathConversion (pieces, queries) _ = piecesConvert pieces queries
  where
    piecesConvert :: [Text] -> H.Query -> PathsAndQueries
    piecesConvert ("api" : rest) qs = (("api" : rest), qs)
    piecesConvert ps qs = (["/"], qs)
