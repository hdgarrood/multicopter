module Game where

import World
import Player

-- Helis only move in 1 dimension, which makes it easier. The top of the screen
-- is y=0; positive is down.
type Position = Int
type Velocity = Int

data Direction = Down | Up

data Heli = Heli
    { heliPlayer           :: Player
    , heliPosition         :: Position
    , heliVelocity         :: Velocity
    , heliDirection        :: Direction
    , heliIsAlive          :: Bool
    , heliCollidedWithWall :: Bool
    , heliQueuedCollisions :: [Collision]
    }

data Collision = Collision
    { collisionVelocity :: Velocity
    , collisionIsAbove  :: Bool
    , collisionHeli     :: Heli
    }

data Game = Game
    { gameWorld   :: World
    , gameHelis   :: [Heli]
    }

data InputData = InputData ()

class GameObject a where
    handleEvents :: InputData -> a -> a
    handleEvents _ = id

    doLogic :: Changes b => a -> Writer [b] a
    doLogic = return

instance GameObject Heli where
    handleEvents inputData heli =
        if heliIsAlive heli
            then (checkForWallCollisions .
                checkForPlayerCollisions .
                changeDirection inputData) heli
            else heli
        where
            -- TODO
            changeDirection inputData heli = heli
            checkForPlayerCollisions heli  = heli
            checkForWallCollisions heli    = heli

    doLogic heli = return heli
