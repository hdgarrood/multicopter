module Game.Constants where

-- for Fay
import Prelude

heliXPosition :: Int
heliXPosition = 50

heliWidth :: Int
heliWidth = 15

heliHeight :: Int
heliHeight = 15

-- how many slices can fit into a world at once
maxSlicesInWorld :: Int
maxSlicesInWorld = 28

-- how wide is a slice
sliceWidth :: Int
sliceWidth = 30

-- how wide is the whole world
-- Subtract one because two slices will always be only partially visible
worldWidth :: Int
worldWidth = (maxSlicesInWorld - 1) * sliceWidth

startingVelocity :: Double
startingVelocity = 5

-- The amount the world speeds up per step
worldAcceleration :: Double
worldAcceleration = 0.001

-- The height of a slice.
sliceHeight :: Int
sliceHeight = 400

-- The mean length of a run of a wall or ceiling at a certain gradient, before
-- another gradient will be chosen.
meanSequenceLength :: Double
meanSequenceLength = 15.0

-- The minimum allowable width of a floor or roof obstacle.
minEdgeWidth :: Int
minEdgeWidth = 20

-- The maximum allowable width of a floor or roof obstacle.
maxEdgeWidth :: Int
maxEdgeWidth = 70

-- The minimum allowable width of a centre-of-slice obstacle.
minObstacleWidth :: Int
minObstacleWidth = 50

-- The maximum allowable width of a centre-of-slice obstacle.
maxObstacleWidth :: Int
maxObstacleWidth = 180

-- The mean number of slices between obstacles.
meanSlicesBetweenObstacles :: Double
meanSlicesBetweenObstacles = 15.0

-- The number of slices at the start of a game which will have no central
-- obstacles.
gracePeriod :: Int
gracePeriod = 20

-- we need a single 0 so that the whole slice is empty, rather than full.
emptySlice :: [Int]
emptySlice = [0]
