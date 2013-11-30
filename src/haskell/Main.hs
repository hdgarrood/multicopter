import Control.Concurrent.STM
import Control.Monad                        (forever, void)
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Writer                 (runWriter)
import Control.Monad.Reader

import Web.Scotty.Trans

import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson                           (encode)
import Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding               (decodeUtf8)
import qualified Data.ByteString as BS hiding (putStrLn)
import qualified Data.ByteString.Char8 as BS (putStrLn)
import qualified Data.ByteString.Lazy as BSL hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS
import Data.FileEmbed                       (embedDir)
import qualified Data.Text.Format as TF

import World
import FileEmbedMiddleware                  (fileEmbed)
import Views
import ServerState
import WebM
import Player

strictToLazy :: BS.ByteString -> BSL.ByteString
strictToLazy = BSL.fromChunks . (: [])

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

-- Maximum cookie age, in seconds
maxCookieAge :: Int
maxCookieAge = 86400

makeCookieString :: Text -> Text -> Text
makeCookieString k v = TF.format "{}={}; Expires={}" (k, v, maxCookieAge)

setCookie :: Text -> Text -> ActionT WebM ()
setCookie k v = setHeader "Set-Cookie" (makeCookieString k v)

startScottyThread :: TVar ServerState -> IO ()
startScottyThread tvar = do
    let runM m = runReaderT (runWebM m) tvar
        runActionToIO = runM

    scottyT 3000 runM runActionToIO $ do
        middleware logStdoutDev
        -- middleware safeStaticDataFiles
        -- middleware $ fileEmbed $(embedDir "src/static")

        get "/" $ do
            redirect "/register"

        get "/register" $ do
            render registrationForm

        post "/register" $ do
            (do name <- param "name"
                player <- webM $ modifyWith (\pr -> addPlayer pr name)
                setCookie "auth_token"
                    (decodeUtf8 $ strictToLazy $ token player)
                redirect "/register")
            `rescue` (\_ -> redirect "/register")

        get "/registered-players" $ do
            players <- webM $ gets getAllPlayers
            render (registeredPlayers players)

main :: IO ()
main = do
    tvar <- newServerState >>= newTVarIO

    -- void $ forkIO $ startGameThread       mstate
    -- void $ forkIO $ startWebSocketsThread mstate
    startScottyThread     tvar
