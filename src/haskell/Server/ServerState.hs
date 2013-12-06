module Server.ServerState where

import Data.Text.Lazy (Text)
import Data.ByteString (ByteString)
import Control.Monad.Random
import Control.Concurrent.STM
import qualified Network.WebSockets as WS

import Server.Player
import Server.GameRepository
import Server.PlayerRepository
import Server.Types
import Game.Types

newServerState :: Rand StdGen ServerState
newServerState = do
    ps <- makePlayerRepository
    gs <- makeGameRepository
    return $
        ServerState { serverPlayers = ps, serverGames = gs }

newServerState' :: IO ServerState
newServerState' = do
    gen <- getSplit
    let (state, _) = runRand newServerState gen
    return state

makeGameJoinInfo :: GameInfo -> WS.Connection -> GameJoinInfo
makeGameJoinInfo info conn = (fst info, snd info, conn)

-- TODO
tryAddPlayerToGame :: GameJoinInfo -> ServerState -> Either Text ServerState
tryAddPlayerToGame info state = Left "game not found"

tryAddPlayerToGameSTM :: GameJoinInfo ->
                         TVar ServerState ->
                         STM (Either Text ())
tryAddPlayerToGameSTM info tvar = do
    state <- readTVar tvar
    case tryAddPlayerToGame info state of
        Left err     -> return $ Left err
        Right state' -> do writeTVar tvar state'
                           return $ Right ()
