module Slice where

import System.Random
import Data.Fixed (mod')

-- A slice of the visible world. Players will spend most of the time across
-- two slices. Has a roof, a floor, and sometimes a third obstacle in the
-- middle.
-- Obstacles are the exact same widths as the slice they occupy.
-- Additionally, they may occupy only one slice.
-- A Slice is represented as a list of Doubles, which are the edges of the
-- contained obstacles. We assume that we're starting inside an obstacle (since
-- we will always have a roof)
-- All the doubles in a Slice should satisfy 0 <= x <= 1 (?)
type Slice = [Double]

sliceHeight :: Double
sliceHeight = 1

-- Is an object occupying the vertical space from a to b colliding with any
-- obstacles in a slice?
isSliceClear :: Slice -> Double -> Double -> Bool
isSliceClear s a b =
    isSliceClear' s False
    where
        isSliceClear' slice state =
            case slice of
             -- we've tested all the switches and the boundary is after them
             -- all, so we're either completely outside or completely inside an
             -- obstacle.
             []     -> state
             (x:xs) ->
                if x < a
                    -- we haven't reached the boundary yet -- recurse and flip
                    -- the state
                    then isSliceClear' xs (not state)
                    else if x < b
                        -- there's a switch inside our boundary, so there must
                        -- be a collision either way.
                        then False
                        -- We're either completely clear of or completely
                        -- inside an obstacle
                        else state

class SliceGen a where
    nextSlice :: a -> (a, Slice)


data StdSliceGen =
    StdSliceGen { -- things that change --
                  prevRoofThickness :: Double
                , prevFloorThickness :: Double
                , slicesToNextObstacle :: Int
                , randomGen :: StdGen
                  -- things that stay the same --
                -- max floor and max roof thickness (they're the same)
                , maxEdgeThickness :: Double
                -- maximum deviation of floor or roof thickness in adjacent
                -- slices
                , maxDeviation :: Double
                , slicesBetweenObstacles :: Int
                } deriving (Show, Read)

instance SliceGen StdSliceGen where
    nextSlice gen =
        -- do it back-to-front so that we can build the list up backwards (for
        -- efficiency)
        makeRoof . makeObstacle . makeFloor $ (gen, [])

-- given a RandomGen Double, a previous thickness, a maximum
-- deviation, and a maxiumum edge thickness, return a new RandomGen
-- Double and an adjusted thickness
-- TODO
adjustThickness :: StdGen -> Double -> Double -> Double -> (StdGen, Double)
adjustThickness rgen th maxD maxT =
    let (delta, rgen') = randomR (0, maxD) rgen
        adjustUntilSmaller a b =
            if a < b
                then a
                else adjustUntilSmaller (a-b) b
        th' = adjustUntilSmaller (th + delta) maxT
    in  (rgen', th')

makeRoof :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
makeRoof (sgen, slice) =
    let rgen = randomGen sgen
        th = prevRoofThickness sgen
        maxDev = maxDeviation sgen
        maxTh = maxEdgeThickness sgen
        (rgen', th') = adjustThickness rgen th maxDev maxTh
        sgen' = sgen { randomGen = rgen', prevRoofThickness = th' }
    in (sgen', th' : slice)

makeFloor :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
makeFloor (sgen, slice) =
    let rgen = randomGen sgen
        th = prevFloorThickness sgen
        maxDev = maxDeviation sgen
        maxTh = maxEdgeThickness sgen
        (rgen', th') = adjustThickness rgen th maxDev maxTh
        sgen' = sgen { randomGen = rgen', prevFloorThickness = th' }
        floorDist = sliceHeight - th'
    in (sgen', floorDist : slice)

makeObstacle :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
makeObstacle (sgen, slice) = (sgen, slice)
    -- let slices = slicesToNextObstacle sgen
    --     makeObstacle' (sgen, slice) =
    --         let maxObstacleHeight = sliceHeight / 2
    --             rgen = randomGen sgen
    --             (rgen', obstacleHeight) = randomR (0, maxObstacleHeight) rgen
    --             maxObstaclePos = sliceHeight - obstacleHeight
    --             (rgen'', obstaclePos) = randomR (0, maxObstaclePos) rgen'
    --             sgen' = sgen
    --                 { randomGen = rgen'',
    --                 , slicesToNextObstacle = slicesBetweenObstacles sgen
    --                 }
    --         in (sgen', obstacleHeight
    -- in
    --     if slices == 0
    --         then makeObstacle' (sgen, slice)
    --         else (sgen { slicesToNextObstacle = slices - 1 }, slice)
