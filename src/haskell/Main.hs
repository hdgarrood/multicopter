import Control.Concurrent.STM

import Server.ScottyApp
import Server.ServerState

main :: IO ()
main = do
    tvar <- newServerState' >>= newTVarIO
    startScottyApp tvar

    -- void $ forkIO $ startGameThread       mstate
    -- void $ forkIO $ startWebSocketsThread mstate

-- webSocketServerPort :: Int
-- webSocketServerPort = 9160

-- startWebSocketsThread :: MVar ServerState -> IO ()
-- startWebSocketsThread state =
--     WS.runServer "0.0.0.0" webSocketServerPort $
--         (\rq -> do
--             WS.acceptRequest rq
--             liftIO $ putStrLn "New connection"
--             pubSub <- liftIO $ fmap snd $ readMVar state
--             WS.subscribe pubSub)

-- stepsPerSecond :: Int
-- stepsPerSecond = 60

-- microsecondsPerStep :: Int
-- microsecondsPerStep = floor (1000000.0 / fromIntegral stepsPerSecond)

-- startGameThread :: MVar ServerState -> IO ()
-- startGameThread state = do
--     forever $ do
--         (world, pubSub)       <- takeMVar state
--         let (world', changes) =  runWriter $ iterateWorld world
--         putMVar state (world', pubSub)

--         threadDelay microsecondsPerStep

--         let message     = encode changes
--         B.putStrLn $ "sending " `B.append` message
--         WS.publish pubSub $ WS.textData $ decodeUtf8 message
