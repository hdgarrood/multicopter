module Game.Game where

import Control.Monad.Writer
import Control.Monad.Random

import Game.Types
import Game.World
import Game.Heli

makeGame :: GameId -> Rand StdGen Game
makeGame gId = do
    world <- makeWorld
    return $ Game
        { gameId    = gId
        , gameWorld = world
        , gameHelis = []
        , gameState = NotStarted
        }

addHeli :: HeliId -> Game -> Game
addHeli hId game = game { gameHelis = newHeli : gameHelis game }
    where newHeli = makeHeli hId

removeHeli :: HeliId -> Game -> Game
removeHeli hId game = game { gameHelis = helis }
    where
        helis = filter ((/= hId) . heliId) $ gameHelis game

start :: Game -> Game
start game = game { gameState = InProgress }

finish :: Game -> Game
finish game = game { gameState = Finished }

isInProgress :: Game -> Bool
isInProgress game = gameState game == InProgress

stepGame :: Game -> InputData -> Writer GameChanges Game
stepGame game input =
    foldl (>>=) (return game) actions
    where
        actions = [stepWorld, stepHelis input, checkFinished]

-- TODO
stepHelis :: InputData -> Game -> Writer GameChanges Game
stepHelis _ g = return g

stepWorld :: Game -> Writer GameChanges Game
stepWorld g = do
    let world = gameWorld g
    let (world', chs) = runWriter (iterateWorld world)
    tell $ toGameChanges chs
    return $ g { gameWorld = world' }

checkFinished :: Game -> Writer GameChanges Game
checkFinished game =
    if shouldFinish game
        then finishGame game
        else return game

finishGame :: Game -> Writer GameChanges Game
finishGame game = do
    tell [GameFinished]
    return $ game { gameState = Finished }

shouldFinish :: Game -> Bool
shouldFinish = all (not . heliIsAlive) . gameHelis
