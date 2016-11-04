{-# LANGUAGE OverloadedStrings #-}

{-|
TODO: document
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

-- Set up a piece of middleware that is used during development. It allows
-- cross-origin requests between the webpack development server and the API.
-- todo: improve documentation
corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just simpleCorsResourcePolicy {
  corsOrigins = Just (["http://localhost:3000"], False),
  corsMethods = ["GET", "PUT", "POST", "OPTIONS"]
}

-- Set up a piece of middleware that allows serving static files from the
-- compiled web front end directory.
-- todo: improve documentation
staticMiddleware :: Middleware
staticMiddleware = staticPolicy (addBase "../web/build/")

-- Set up a piece of middleware that allows request rewriting. It is used to
-- route all non-API requests to the root route, where the web front end
-- handles routing.
-- todo: improve documentation
rewriteMiddleware :: Middleware
rewriteMiddleware = rewritePureWithQueries pathConversion

-- | The 'pathConversion' function rewrites URLS based on certain criteria.
-- Requests to endpoints starting with api are simply passed through as-is,
-- while all other requests are redirected to the root route.
pathConversion :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
pathConversion (pieces, queries) _ = piecesConvert pieces queries
  where
    piecesConvert :: [Text] -> H.Query -> PathsAndQueries
    piecesConvert ["api", method, trail] qs = (["api", method, trail], qs)
    piecesConvert ps qs = (["/"], qs)
