module Server.StaticFileMiddleware where

import qualified Network.Wai as W
import Network.Wai.Application.Static

staticFiles :: W.Middleware
staticFiles app req =
    maybe (app req)
          staticFilesApp
          (modifyReq req)

modifyReq :: W.Request -> Maybe W.Request
modifyReq req = case W.pathInfo req of
    "static":xs -> Just $ req { W.pathInfo = xs }
    _           -> Nothing

staticFilesApp :: W.Application
staticFilesApp = staticApp $ defaultFileServerSettings "src/static"
