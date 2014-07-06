module Server.WebSocketsApp where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Aeson
import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.URI as URI
import qualified Web.Cookie as C
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

import Server.Types
import Server.GameRepository
import Game.Types
import Game.Game
import Server.ServerState
import Conversion
import EitherUtils
import Logging

threadDelaySec :: Double -> IO ()
threadDelaySec = threadDelay . floor . (* oneMillion)
    where
        oneMillion = 10 ^ (6 :: Int)

multicopterWebSocketsMiddleware ::
    TVar ServerState -> Wai.Application -> Wai.Application
multicopterWebSocketsMiddleware tvar =
    let app = multicopterWebSocketsApp tvar
    in  WS.websocketsOr WS.defaultConnectionOptions app

-- TODO: Remove players from games if they disconnect
multicopterWebSocketsApp :: TVar ServerState -> WS.ServerApp
multicopterWebSocketsApp state = \pc ->
        either (reject pc) (accept pc) (getGameInfo pc)
    where
        reject pc err = do
            putLog $ "rejecting a websocket request: " ++ convert err
            WS.rejectRequest pc err

        accept pc info = do
            putLog $ "accepting a websocket request: " ++ show info
            conn <- WS.acceptRequest pc
            let info' = makeGameJoinInfo info conn
            tryAddPlayerToGame' info' state
            blockIndefinitely

        blockIndefinitely = forever $ threadDelaySec 10

getGameInfo :: WS.PendingConnection -> Either ByteString GameInfo
getGameInfo req = do
    let reqHead = WS.pendingRequest req
    gId <- getGameId    reqHead
    tok <- getAuthToken reqHead
    return (gId, tok)

toString :: ByteString -> String
toString = map (chr . fromIntegral) . B.unpack

readEither :: Read a => ByteString -> Either ByteString a
readEither s = case [ x | (x,"") <- reads (toString s) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"

getGameId :: WS.RequestHead -> Either ByteString GameId
getGameId = getGameId' . pathInfo . URI.decodePath . WS.requestPath
    where pathInfo = fst

getGameId' :: [Text] -> Either ByteString GameId
getGameId' ("ws":"games":x:[]) =
    withLeft "game not found" . fmap GameId . readEither $ convert x
getGameId' _                  = Left "game not found"

getAuthToken :: WS.RequestHead -> Either ByteString ByteString
getAuthToken = fromMaybe "auth token not supplied" .
    getCookie "auth_token" . WS.requestHeaders
    where
        getCookie name headers = do
            cookies <- fmap C.parseCookies $ lookup "cookie" headers
            lookup name cookies

tryAddPlayerToGame' :: GameJoinInfo ->
                       TVar ServerState ->
                       IO ()
tryAddPlayerToGame' info state = do
    result <- atomically $ tryAddPlayerToGameSTM info state
    case result of
        Right () -> return ()
        Left err -> let (_, _, conn) = info
                    in WS.sendClose conn err

stepsPerSecond :: Int
stepsPerSecond = 60

secondsPerStep :: Double
secondsPerStep = 1 / fromIntegral stepsPerSecond

broadcast :: WS.WebSocketsData a => Clients -> a -> IO ()
broadcast cs msg = forM_ cs $ flip WS.sendTextData msg . fst

-- TODO put each running game's data in a separate TVar so that other parts of
-- the app will not affect it.
startGameThread :: TVar ServerState -> IO ()
startGameThread state = do
    forever $ do
        updateActions <- atomically $ do
            games <- fmap (getAllGames . serverGames) $ readTVar state
            results <- forM games $ \(game, clients) -> do
                let (game', changes) = runWriter $ stepGame game InputData
                let message = encode changes
                let update = do
                    putLog $ "sending: " ++ convert message
                    broadcast clients message
                return ((game', clients), update)

            let games'        = map fst results
            let updateActions = map snd results

            modifyTVar state $ \s ->
                s { serverGames = reconstructGames games' (serverGames s) }

            return updateActions

        sequence_ updateActions
        threadDelaySec secondsPerStep

