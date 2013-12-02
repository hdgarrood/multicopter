module Game where

import Control.Monad.Writer
import Data.Default

import Game.World

-- Helis only move in 1 dimension, which makes it easier. The top of the screen
-- is x=0; positive is down.
type Position = Int
type Velocity = Int

data Direction = Down | Up

data Heli = Heli
    { heliPosition  :: Position
    , heliVelocity  :: Velocity
    , heliDirection :: Direction
    , heliIsAlive   :: Bool
    }

data HeliEvents = HeliEvents
    { heliNewDirection     :: Maybe Direction
    , heliCollidedWithWall :: Bool
    , heliQueuedCollisions :: [Collision]
    }   

instance Default HeliEvents where
    def = HeliEvents
        { heliNewDirection     = Nothing
        , heliCollidedWithWall = False
        , heliQueuedCollisions = []
        }

data Collision = Collision
    { collisionVelocity :: Velocity
    , collisionIsAbove  :: Bool
    , collisionOther    :: Heli
    }

data Game = Game
    { gameWorld   :: World
    , gameHelis   :: [Heli]
    }

-- TODO
data InputData = InputData ()
data Change = Change ()

data GameObject = WorldObject World
                | HeliObject Heli


type GameObjectEvents = Maybe HeliEvents
   
-- Given all the input data since the last step and a game object, return a thing
-- which contains all the information needed for that object's doLogic call later this
-- step.
handleEvents :: InputData -> GameObject -> GameObjectEvents
handleEvents _ (WorldObject _) = Nothing
handleEvents i (HeliObject h) = Just $ heliHandleEvents i h

-- Called once per game step to change the state of a GameObject. The [Change]
-- will be sent back to all clients so that they can adjust their state too.
doLogic :: GameObject -> GameObjectEvents -> Writer [Change] GameObject
doLogic obj evs = return obj

-- Implementation for Helis
heliHandleEvents :: InputData -> Heli -> HeliEvents
heliHandleEvents inputData heli =
    if heliIsAlive heli
        then events
        else def
    where
    events = HeliEvents
        { heliNewDirection     = newDir
        , heliCollidedWithWall = collidedWithWall
        , heliQueuedCollisions = playerCollisions
        }

    newDir           = getNewDirection     inputData heli
    collidedWithWall = getWallCollision    inputData heli
    playerCollisions = getPlayerCollisions inputData heli

    -- TODO
    getNewDirection _ _     = Nothing
    getWallCollision _ _    = False
    getPlayerCollisions _ _ = []
