import Control.Concurrent.STM
import Control.Monad                        (forever, void)
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Writer                 (runWriter)
import Control.Monad.Reader

import Web.Scotty.Trans

import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson                           (encode)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding              (decodeUtf8)
import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS
import Data.FileEmbed                       (embedDir)

import World
import FileEmbedMiddleware                  (fileEmbed)
import Views
import ServerState
import WebM

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

startScottyThread :: TVar ServerState -> IO ()
startScottyThread tvar = do
    let runM m = runReaderT (runWebM m) tvar
        runActionToIO = runM

    scottyT 3000 runM runActionToIO $ do
        -- middleware logStdoutDev
        -- middleware safeStaticDataFiles
        middleware $ fileEmbed $(embedDir "src/static")

        get "/" $ do
            redirect "/register"

        get "/register" $ do
            render RegistrationForm

        post "/register" $ do
            -- TODO
            redirect "/register"

main :: IO ()
main = do
    tvar <- newServerState >>= newTVarIO

    -- void $ forkIO $ startGameThread       mstate
    -- void $ forkIO $ startWebSocketsThread mstate
    startScottyThread     tvar
