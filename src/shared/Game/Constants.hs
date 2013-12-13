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

