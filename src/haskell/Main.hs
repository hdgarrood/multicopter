import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (void)

import Server.ScottyApp
import Server.WebSocketsApp
import Server.ServerState

-- TODO: check if `runInUnboundThread` will help performance
main :: IO ()
main = do
    tvar <- newServerState' >>= newTVarIO

    -- TODO: Catch MalformedRequests and handle sensibly
    void $ forkIO $ startWebSocketsApp tvar
    startScottyApp tvar
