{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Monad.IO.Class

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

import World
import SafeStaticDataFileMiddleware         (safeStaticDataFiles)

-- for now, a client is just a sink
type Client = WS.Sink WS.Rfc6455
type ServerState = (World, [Client])

newServerState :: ServerState
newServerState = (makeWorld, [])

broadcast :: ByteString -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn $ "broadcasting: " ++ message
    mapM_ (\s -> WS.sendSink s $ WS.binaryData message) clients

webSocketServerPort :: Int
webSocketServerPort = 9160

startWebSocketsThread :: MVar ServerState -> IO ()
startWebSocketsThread state = do
    WS.runServer "0.0.0.0" webSocketServerPort $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Rfc6455 ()
application state rq = do
    WS.acceptRequest rq
    T.putStrLn "New connection"
    forever $ do

main :: IO ()
main = do
    state <- newMVar newServerState

    forkIO $ startWebSocketsThread state

    scotty 3000 $ do
        middleware logStdoutDev
        middleware safeStaticDataFiles

        get "/" $ do
            redirect "/static/index.html"
