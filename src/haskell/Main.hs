import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Network.Wai.Handler.Warp

import Server.ScottyApp
import Server.WebSocketsApp
import Server.ServerState

-- TODO: check if `runInUnboundThread` will help performance
main :: IO ()
main = do
    let port = 3000
    putStrLn $ "multicopter: starting on port " ++ show port ++ "..."

    tvar <- newServerState' >>= newTVarIO

    scottyAppPart <- multicopterScottyApp tvar
    let withWebSocketsPart = multicopterWebSocketsMiddleware tvar
    let fullApp = withWebSocketsPart scottyAppPart
    let settings = setPort port
                 . setFdCacheDuration 0 -- work around wai issue #210
                 $ defaultSettings

    -- TODO: websockets: Catch MalformedRequests and handle sensibly
    void . forkIO $ runSettings settings fullApp

    -- TODO: fix "send: resource vanished (broken pipe)" crash
    startGameThread tvar
