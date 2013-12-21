module Server.WebSocketsApp where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Aeson
import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types.URI as URI
import qualified Web.Cookie as C
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)

import Server.Types
import Server.GameRepository
import Game.Types
import Game.Game
import Server.ServerState
import Conversion
import EitherUtils

webSocketServerPort :: Int
webSocketServerPort = 9160

startWebSocketsApp :: TVar ServerState -> IO ()
startWebSocketsApp state = do
    void $ forkIO $ startWebSocketsThread state
    startGameThread state

startWebSocketsThread :: TVar ServerState -> IO ()
startWebSocketsThread state =
    WS.runServer "0.0.0.0" webSocketServerPort $
        \pc -> do
            let reqHead = WS.pendingRequest pc

            case getGameInfo reqHead of
                Right info -> do conn <- WS.acceptRequest pc
                                 let info' = makeGameJoinInfo info conn
                                 tryAddPlayerToGame' info' state
                Left err   -> WS.rejectRequest pc err

getGameInfo :: WS.RequestHead -> Either ByteString GameInfo
getGameInfo reqHead = do
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

microsecondsPerStep :: Int
microsecondsPerStep = floor (oneMillion / fromIntegral stepsPerSecond)
    where
        oneMillion :: Double
        oneMillion = (10 :: Double) ^ (6 :: Int)

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
                let update = broadcast clients message
                return ((game', clients), update)

            let games'        = map fst results
            let updateActions = map snd results

            modifyTVar state $ \s ->
                s { serverGames = reconstructGames games' (serverGames s) }

            return updateActions

        threadDelay microsecondsPerStep
        sequence_ updateActions

