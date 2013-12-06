module Server.WebSocketsApp where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types.URI as URI
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

import Server.Types
import Game.Types
import Server.ServerState
import Conversion

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

withLeft :: a -> Either a b -> Either a b
withLeft x = either (const $ Left x) Right

getGameId :: WS.RequestHead -> Either ByteString GameId
getGameId = getGameId' . pathInfo . URI.decodePath . WS.requestPath
    where pathInfo = fst

getGameId' :: [Text] -> Either ByteString GameId
getGameId' ("ws":"games":x:[]) =
    withLeft "game not found" . fmap GameId . readEither $ convert x
getGameId' _                  = Left "game not found"

getAuthToken :: WS.RequestHead -> Either ByteString ByteString
getAuthToken = getAuthToken' . query . URI.decodePath . WS.requestPath
    where query = snd

getAuthToken' :: URI.Query -> Either ByteString ByteString
getAuthToken' q = case lookup "auth_token" q of
                    Just (Just x) -> Right x
                    _             -> Left "auth token not found"

tryAddPlayerToGame' :: GameJoinInfo ->
                       TVar ServerState ->
                       IO ()
tryAddPlayerToGame' info state = do
    result <- atomically $ tryAddPlayerToGameSTM info state
    case result of
        Right () -> return ()
        Left err -> let (_, _, conn) = info
                    in WS.sendClose conn err

-- stepsPerSecond :: Int
-- stepsPerSecond = 60

-- microsecondsPerStep :: Int
-- microsecondsPerStep = floor (1000000.0 / fromIntegral stepsPerSecond)

startGameThread :: TVar ServerState -> IO ()
startGameThread = const (return ())
-- startGameThread :: TVar ServerState -> IO ()
-- startGameThread state = do
--     forever $ do
--         atomically $ do
--             (world, pubSub)       <- readTVar state
--             let (world', changes) =  runWriter $ iterateWorld world
--             writeTVar state (world', pubSub)

--         threadDelay microsecondsPerStep

--         let message     = encode changes
--         B.putStrLn $ "sending " `B.append` message
--         WS.publish pubSub $ WS.textData $ decodeUtf8 message
