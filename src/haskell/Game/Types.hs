module Game.Types where

import Data.Aeson
import qualified Data.Text.Lazy as T
import System.Random
import Control.Monad.Random

-- === GAME ===
data Game = Game
    { gameWorld   :: World
    , gameHelis   :: [Heli]
    }

-- === WORLD ===
data World =
    World { slices           :: [Slice]
          , sliceGen         :: StdSliceGen
          -- how far along the world has moved since the last slice was added
          , offset           :: Double
          -- how far the slices move per step. This will slowly increase over
          -- time.
          , velocity         :: Double
          , randomGen        :: StdGen
          } deriving (Show, Read)

-- Any piece of information which we send back to clients regarding a change of
-- state in the world is packaged up as a WorldChange.
data WorldChange = SliceAdded Slice -- A slice was added
                 | SlicesMoved Int -- Tells how far the slices moved
                   deriving (Show)

instance ToJSON WorldChange where
    toJSON (SliceAdded slice) =
        object [ "type" .= ("sliceAdded"  :: T.Text)
               , "data" .= slice
               ]
    toJSON (SlicesMoved dist) =
        object [ "type" .= ("slicesMoved" :: T.Text)
               , "data" .= dist
               ]

type WorldChanges = [WorldChange]

-- === HELI ===
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

data HeliEvent = ChangedDirection Direction
               | CollidedWithWall
               | CollidedWithHeli Collision

-- TODO: less naive implementation
type HeliEvents = [HeliEvent]

data Collision = Collision
    { collisionVelocity :: Velocity
    , collisionIsAbove  :: Bool
    , collisionOther    :: Heli
    }

-- Signals that a heli can 'send'.
data HeliSignal = StartTheGame
                | ChangeDirection Direction

-- TODO: less naive implementation
type HeliInputData = [HeliSignal]

-- Any information sent back to clients regarding a change of state of a heli
-- is sent back as a HeliChange.
data HeliChange = HeliMoved Int
                | HeliCrashed

-- === SLICE ===
-- A slice of the visible world. Players will spend most of the time across
-- two slices. Has a roof, a floor, and sometimes a third obstacle in the
-- middle.
--
-- Obstacles are the exact same widths as the slice they occupy.
--
-- A Slice is represented as a list of Ints, which are the edges of the
-- contained obstacles.
--
-- When checking whether a particular space in a slice is clear, we assume that
-- we're starting inside an obstacle (since we will always have a roof).
type Slice = [Int]

class SliceGen g where
    nextSlice :: g -> Rand StdGen (Slice, g)

-- A StdSliceGen can be thought of as a machine which takes randomness and
-- turns it into slices. Note than no randomness is stored within the
-- StdSliceGen; it must be provided in order for it to produce slices.
data StdSliceGen =
    StdSliceGen
        { roofWidthGen  :: EdgeWidthGen
        , floorWidthGen :: EdgeWidthGen
        , obstacleGen   :: ObstacleGen
        } deriving (Show, Read)

data EdgeWidthGen =
    EdgeWidthGen
        { remainingSlicesInSequence :: Int
        , currentGradient           :: Int
        , currentWidth              :: Int
        } deriving (Show, Read)

data ObstacleGen =
    ObstacleGen
        { slicesUntilNextObstacle :: Int
        } deriving (Show, Read)
