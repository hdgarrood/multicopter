{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Web.Scotty                           (scotty,
                                             middleware,
                                             get,
                                             redirect,
                                             json)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.Static        (staticPolicy,
                                             hasPrefix,
                                             noDots,
                                             (>->))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson                           (encode)

import World

-- Ensures that people can only get files within ./static
safeStatic :: Middleware
safeStatic =
    staticPolicy $
        hasPrefix "static/" >->
        noDots

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware safeStatic

    world <- liftIO $ makeWorld >>= newMVar

    get "/" $ do
        redirect "/static/index.html"

    get "/iterate" $ do
        wl <- liftIO $ takeMVar world
        let wl' = iterateWorld wl
        liftIO $ putMVar world wl'
        json wl'
