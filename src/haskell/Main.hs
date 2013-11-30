import Control.Concurrent.STM
import Control.Monad                        (forever, void)
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Writer                 (runWriter)
import Control.Monad.Reader

import           Network.Wai                    (Middleware)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Util.PubSub as WS
import           Web.Scotty.Trans
import           Web.Cookie

import Data.Aeson (encode)

import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Builder  as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Format        as TF

import qualified Data.ByteString            as BS hiding  (putStrLn)
import qualified Data.ByteString.Char8      as BS         (putStrLn)
import qualified Data.ByteString.Lazy       as BSL hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL        (putStrLn)
import qualified Blaze.ByteString.Builder   as Builder

import Data.FileEmbed (embedDir)

import World
import FileEmbedMiddleware                  (fileEmbed)
import Views
import ServerState
import WebM
import Player

strictToLazy :: BS.ByteString -> BSL.ByteString
strictToLazy = BSL.fromChunks . (: [])

lazyToStrict :: BSL.ByteString -> BS.ByteString
lazyToStrict = BS.concat . BSL.toChunks

makeCookie :: Text -> Text -> SetCookie
makeCookie n v = def { setCookieName = n', setCookieValue = v' }
    where
        conv = lazyToStrict . T.encodeUtf8
        n' = conv n
        v' = conv v

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = T.decodeUtf8 . Builder.toLazyByteString . renderSetCookie

setCookie :: Text -> Text -> ActionT WebM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

startScottyThread :: TVar ServerState -> IO ()
startScottyThread tvar = do
    let runM m = runReaderT (runWebM m) tvar
        runActionToIO = runM

    scottyT 3000 runM runActionToIO $ do
        -- middleware logStdoutDev
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
                    (T.decodeUtf8 $ strictToLazy $ token player)
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
