module Server.ServerState where

import Control.Exception (assert)
import Control.Monad.Random
import Control.Concurrent.STM
import Data.Maybe (isJust, fromJust)
import Data.Text.Lazy (Text)
import qualified Network.WebSockets as WS

import Server.GameRepository
import Server.PlayerRepository
import Server.Types
import Game.Game
import EitherUtils

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

tryAddPlayerToGame :: GameJoinInfo -> ServerState -> Either Text ServerState
tryAddPlayerToGame (gId, tok, conn) state = do
    player <- fromMaybe "auth unsuccessful" $
                getPlayerBy (Token tok) (serverPlayers state)

    (game, clients) <- fromMaybe "game not found" $
                        getGameById gId (serverGames state)

    return $ go (player, game, clients)

    where
        go (player, game, clients) =
            let (hId, game') = addHeli game (playerName player)
                client       = (conn, hId)
                games_       = modifyGame gId
                                    (game', client : clients)
                                    (serverGames state)
                games'       = fromJust $ assert (isJust games_) games_
            in  state { serverGames = games' }

tryAddPlayerToGameSTM :: GameJoinInfo ->
                         TVar ServerState ->
                         STM (Either Text ())
tryAddPlayerToGameSTM info tvar = do
    state <- readTVar tvar
    case tryAddPlayerToGame info state of
        Left err     -> return $ Left err
        Right state' -> do writeTVar tvar state'
                           return $ Right ()
