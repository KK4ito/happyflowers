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
module HappyFlowers.API.Middleware
    (
      -- * Operations
      staticMiddleware
    , rewriteMiddleware
    ) where

import Data.Text                      (Text)
import Network.HTTP.Types             as H
import Network.Wai                    (Middleware)
import Network.Wai.Middleware.Rewrite (PathsAndQueries, rewritePureWithQueries)
import Network.Wai.Middleware.Static  (addBase, staticPolicy)

-- | sets up a piece of middleware that allows serving static files.
staticMiddleware :: Middleware
staticMiddleware = staticPolicy (addBase "../web/build/")

-- | sets up a piece of middleware that allows request rewriting. Used to route
-- all non-API requests to the root route, where the web front end handles
-- routing.
rewriteMiddleware :: Middleware
rewriteMiddleware = rewritePureWithQueries pathConversion

-- | rewrites URLS based on path and query strings.
pathConversion :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
pathConversion (pieces, queries) _ = piecesConvert pieces queries
    where
        piecesConvert :: [Text] -> H.Query -> PathsAndQueries
        piecesConvert ("api" : rest) qs = (("api" : rest), qs)
        piecesConvert ps qs = (["/"], qs)
