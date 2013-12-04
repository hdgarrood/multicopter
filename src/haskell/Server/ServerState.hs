module Server.ServerState where

import Control.Monad.Random

import Server.Player
import Server.GameRepository

data ServerState = ServerState
    { serverPlayers :: PlayerRepository
    , serverGames   :: GameRepository
    }

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
