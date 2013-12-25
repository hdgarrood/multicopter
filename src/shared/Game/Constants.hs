module Game.Constants where

import Data.Aeson
import qualified Data.Text as T
import Language.Haskell.Extract

-- All constants, as a JavaScript object value, mapping names to values.
allConstants :: Value
allConstants = object $
    $(functionExtractorMap "^c_" [| \k v -> (T.drop 2 k) .= v |])

c_heliXPosition :: Int
c_heliXPosition = 50

c_heliWidth :: Int
c_heliWidth = 15

c_heliHeight :: Int
c_heliHeight = 15

-- how many slices can fit into a world at once
c_maxSlicesInWorld :: Int
c_maxSlicesInWorld = 28

-- how wide is a slice
c_sliceWidth :: Int
c_sliceWidth = 30

-- how wide is the whole world
-- Subtract one because two slices will always be only partially visible
c_worldWidth :: Int
c_worldWidth = (c_maxSlicesInWorld - 1) * c_sliceWidth

-- how high is the whole world
c_worldHeight :: Int
c_worldHeight = c_sliceHeight

c_startingVelocity :: Double
c_startingVelocity = 5

-- The amount the world speeds up per step
c_worldAcceleration :: Double
c_worldAcceleration = 0.001

-- The height of a slice.
c_sliceHeight :: Int
c_sliceHeight = 400

-- The mean length of a run of a wall or ceiling at a certain gradient, before
-- another gradient will be chosen.
c_meanSequenceLength :: Double
c_meanSequenceLength = 15.0

-- The minimum allowable width of a floor or roof obstacle.
c_minEdgeWidth :: Int
c_minEdgeWidth = 20

-- The maximum allowable width of a floor or roof obstacle.
c_maxEdgeWidth :: Int
c_maxEdgeWidth = 70

-- The minimum allowable width of a centre-of-slice obstacle.
c_minObstacleWidth :: Int
c_minObstacleWidth = 50

-- The maximum allowable width of a centre-of-slice obstacle.
c_maxObstacleWidth :: Int
c_maxObstacleWidth = 180

-- The mean number of slices between obstacles.
c_meanSlicesBetweenObstacles :: Double
c_meanSlicesBetweenObstacles = 15.0

-- The number of slices at the start of a game which will have no central
-- obstacles.
c_gracePeriod :: Int
c_gracePeriod = 20

-- we need a single 0 so that the whole slice is empty, rather than full.
c_emptySlice :: [Int]
c_emptySlice = [0]
