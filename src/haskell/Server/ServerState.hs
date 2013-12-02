module Server.ServerState where

import Server.Player
import Game

type ServerState = PlayerRepository

newServerState :: IO ServerState
newServerState = do
    playerRepo <- makePlayerRepository
    return playerRepo
