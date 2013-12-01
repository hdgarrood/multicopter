module ServerState where

import Player
import Game

type ServerState = (PlayerRepository, GameRepository)

newServerState :: IO ServerState
newServerState = do
    playerRepo <- makePlayerRepository
    gameRepo   <- makeGameRepository
    return (playerRepo, gameRepo)
