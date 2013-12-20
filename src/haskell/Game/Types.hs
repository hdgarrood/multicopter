module Game.Types where

import Data.Aeson
import Data.Text.Lazy (Text)
import System.Random

-- === GAME ===
newtype GameId = GameId Int deriving (Eq, Ord, Show, Enum)

unGameId :: GameId -> Int
unGameId (GameId x) = x

data Game = Game
    { gameId         :: GameId
    , gameWorld      :: World
    , gameNextHeliId :: HeliId
    , gameHelis      :: [Heli]
    , gameState      :: GameState
    }

data GameState = NotStarted
               | InProgress
               | Finished
               deriving (Show, Eq)

data GameChange = GameStarted
                | GameFinished
                | WC WorldChange
                | HC HeliChange

instance ToJSON GameChange where
    toJSON GameStarted = object [ "type" .= ("GameStarted" :: Text)
                                ]

    toJSON GameFinished = object [ "type" .= ("GameFinished" :: Text)
                                 ]

    toJSON (WC wc) = object [ "type" .= ("WorldChange" :: Text)
                            , "data" .= toJSON wc
                            ]

    toJSON (HC hc) = object [ "type" .= ("HeliChange" :: Text)
                            , "data" .= toJSON hc
                            ]

type GameChanges = [GameChange]

class ToGameChanges a where
    toGameChanges :: a -> GameChanges

data InputData = InputData

-- === WORLD ===
data World =
    World { worldSlices           :: [Slice]
          , worldSliceGen         :: SliceGen
          -- how far along the world has moved since the last slice was added
          , worldOffset           :: Double
          -- how far the slices move per step. This will slowly increase over
          -- time.
          , worldVelocity         :: Double
          , worldRandomGen        :: StdGen
          } deriving (Show, Read)

-- Any piece of information which we send back to clients regarding a change of
-- state in the world is packaged up as a WorldChange.
data WorldChange = SliceAdded Slice -- A slice was added
                 | SlicesMoved Int -- Tells how far the slices moved
                   deriving (Show)

instance ToJSON WorldChange where
    toJSON (SliceAdded slice) =
        object [ "type" .= ("sliceAdded"  :: Text)
               , "data" .= slice
               ]
    toJSON (SlicesMoved dist) =
        object [ "type" .= ("slicesMoved" :: Text)
               , "data" .= dist
               ]

type WorldChanges = [WorldChange]

instance ToGameChanges WorldChanges where
    toGameChanges = map WC

-- === HELI ===
-- Helis only move in 1 dimension, which makes it easier. The top of the screen
-- is x=0; positive is down.
newtype HeliId = HeliId Int deriving (Eq, Ord, Show, Enum)
type Position = Int
type Velocity = Int
data Direction = Down | Up

data Heli = Heli
    { heliId        :: HeliId
    , heliName      :: Text
    , heliPosition  :: Position
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

instance ToJSON HeliChange where
    toJSON (HeliMoved x) =
        object [ "type" .= ("HeliMoved" :: Text)
               , "data" .= x
               ]

    toJSON HeliCrashed =
        object [ "type" .= ("HeliCrashed" :: Text)
               ]

-- TODO: less naive implementation?
type HeliChanges = [HeliChange]

instance ToGameChanges HeliChanges where
    toGameChanges = map HC

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

-- A SliceGen can be thought of as a machine which takes randomness and turns
-- it into slices. Note than no randomness is stored within the SliceGen; it
-- must be provided in order for it to produce slices.
data SliceGen =
    SliceGen
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
