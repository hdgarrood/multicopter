{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent                   (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad                        (forever, void)
import Control.Monad.IO.Class               (liftIO)

import Web.Scotty                           (scotty,
                                             middleware,
                                             get,
                                             redirect,
                                             json)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson                           (encode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS

import World
import SafeStaticDataFileMiddleware         (safeStaticDataFiles)

-- A server state is
type ServerState = (World, WS.PubSub WS.Hybi10)

newServerState :: IO ServerState
newServerState = do
    world  <- makeWorld
    pubSub <- WS.newPubSub
    return (world, pubSub)

webSocketServerPort :: Int
webSocketServerPort = 9160

startWebSocketsThread :: MVar ServerState -> IO ()
startWebSocketsThread state =
    WS.runServer "0.0.0.0" webSocketServerPort $
        (\rq -> do
            WS.acceptRequest rq
            liftIO $ putStrLn "New connection"
            pubSub <- liftIO $ fmap snd $ readMVar state
            WS.subscribe pubSub)

microsecondsPerStep :: Int
microsecondsPerStep = 250 * 1000

startGameThread :: MVar ServerState -> IO ()
startGameThread state = do
    forever $ do
        (world, pubSub) <- takeMVar state
        let world'      =  iterateWorld world
        putMVar state (world', pubSub)

        threadDelay microsecondsPerStep

        let message     = encode world'
        B.putStrLn $ "sending " `B.append` message
        WS.publish pubSub $ WS.binaryData message

startScottyThread :: MVar ServerState -> IO ()
startScottyThread state =
    scotty 3000 $ do
        -- middleware logStdoutDev
        middleware safeStaticDataFiles

        get "/" $ do
            redirect "/static/index.html"
main :: IO ()
main = do
    state  <- newServerState
    mstate <- newMVar state

    void $ forkIO $ startGameThread       mstate
    void $ forkIO $ startWebSocketsThread mstate
    startScottyThread     mstate
