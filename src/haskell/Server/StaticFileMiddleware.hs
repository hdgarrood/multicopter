module Server.StaticFileMiddleware where

import Network.Wai
import Network.Wai.Middleware.Static

staticFiles :: Middleware
staticFiles = staticPolicy multicopterPolicy

multicopterPolicy :: Policy
multicopterPolicy = noDots >-> hasPrefix "static" >-> addBase "src"
