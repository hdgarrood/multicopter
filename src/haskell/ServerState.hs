module ServerState where

import Player

type ServerState = PlayerRepository

newServerState :: IO ServerState
newServerState = do
    playerRepo <- makePlayerRepository
    return playerRepo
