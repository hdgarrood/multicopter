import Control.Concurrent.STM
import Control.Monad                        (forever, void)
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Writer                 (runWriter)
import Control.Monad.Reader

import           Network.Wai
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Util.PubSub as WS
import           Network.HTTP.Types
import           Web.Scotty.Trans
import           Web.Cookie

import Data.Aeson (encode)
import Data.Maybe
import Data.Monoid

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
import Conversion

makeCookie :: Text -> Text -> SetCookie
makeCookie n v = def { setCookieName = n', setCookieValue = v' }
    where
        n' = convert n
        v' = convert v

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = convert . renderSetCookie

setCookie :: Text -> Text -> ActionT WebM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

-- Specify something that should always happen before each request. The given
-- ActionT WebM() should probably contain a 'next' so that the rest of the
-- application can be reached.
beforehand :: ActionT WebM () -> ScottyT WebM ()
beforehand = matchAny (function
    (\req -> Just [("path", convert $ rawPathInfo req)]))

ensureAuthenticated :: ActionT WebM ()
ensureAuthenticated = do
    authed <- isAuthenticated
    if authed
        then next
        else do
            path <- param "path" :: ActionT WebM Text
            case path of
                "/register" -> handleRegistration
                _           -> redirect "/register"

getAuthToken :: ActionT WebM (Maybe Token)
getAuthToken = do
    cookieHeader <- reqHeader "Cookie"
    return $ fmap Token (extract cookieHeader)
    where
        extract :: Maybe Text -> Maybe BS.ByteString
        extract = join . fmap (lookup "auth_token" . parseCookies . convert)

getCurrentPlayer :: ActionT WebM (Maybe Player)
getCurrentPlayer = do
    maybeToken <- getAuthToken
    case maybeToken of
        Nothing    -> return Nothing
        Just token -> webM $ gets (getPlayerByToken token)

-- The call to 'fromJust' is mostly acceptable because any code that calls this
-- will only be executed after we've checked that someone's logged in.
getCurrentPlayer' :: ActionT WebM Player
getCurrentPlayer' = fmap fromJust getCurrentPlayer

-- TODO: Retrieve the player associated with an auth token once per request and
-- save the result somewhere.
isAuthenticated :: ActionT WebM Bool
isAuthenticated = fmap isJust getCurrentPlayer

handleRegistration :: ActionT WebM ()
handleRegistration = do
    req <- request
    case requestMethod req of
        "GET"  -> render registrationForm
        "POST" -> (do name <- param "name"
                      player <- webM $ modifyWith (addPlayer name)
                      setCookie "auth_token" (convert $ token player)
                      redirect "/")
                  `rescue` (\_ -> redirect "/register")
        _      -> status methodNotAllowed405

startScottyThread :: TVar ServerState -> IO ()
startScottyThread tvar = do
    let runM m = runReaderT (runWebM m) tvar
        runActionToIO = runM

    scottyT 3000 runM runActionToIO $ do
        -- middleware logStdoutDev
        -- middleware safeStaticDataFiles
        -- middleware $ fileEmbed $(embedDir "src/static")

        beforehand ensureAuthenticated

        get "/" $ do
            player <- getCurrentPlayer'
            html $ "hooray! you're logged in as " `mappend`
                (name player) `mappend`
                ". <a href=/registered-players>registered players</a>"

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
