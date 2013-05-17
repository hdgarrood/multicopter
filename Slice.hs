module Slice where

import System.Random
import Data.Fixed (mod')

import Utils

-- A slice of the visible world. Players will spend most of the time across
-- two slices. Has a roof, a floor, and sometimes a third obstacle in the
-- middle.
-- Obstacles are the exact same widths as the slice they occupy.
-- Additionally, they may occupy only one slice.
-- A Slice is represented as a list of Ints, which are the edges of the
-- contained obstacles. We assume that we're starting inside an obstacle (since
-- we will always have a roof)
-- All the doubles in a Slice should satisfy 0 <= x <= 1 (?)
type Slice = [Int]

-- Is an object occupying the vertical space from a to b colliding with any
-- obstacles in a slice?
isSliceClear :: Slice -> Int -> Int -> Bool
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

-- a definition of a slice -- parameter to a SliceGen
data SliceDef =
    SliceDef 
        { edgeThicknessRange :: (Int, Int)
        , maxDeviation :: Int
        , slicesBetweenObstacles :: Int
        , height :: Int
        } deriving (Show, Read)

data StdSliceGen =
    StdSliceGen { prevRoofThickness :: Int
                , prevFloorThickness :: Int
                , slicesToNextObstacle :: Int
                , randomGen :: StdGen
                , sliceDefinition :: SliceDef
                } deriving (Show, Read)

mkStdSliceGen :: SliceDef -> IO StdSliceGen
mkStdSliceGen def = do
    gen <- getStdGen'
    return StdSliceGen
        { prevRoofThickness = 0
        , prevFloorThickness = 0
        , slicesToNextObstacle = 100
        , randomGen = gen
        , sliceDefinition = def
        }

-- instance SliceGen StdSliceGen where
--     nextSlice gen =
--         -- do it back-to-front so that we can build the list up backwards (for
--         -- efficiency)
--         makeRoof . makeObstacle . makeFloor $ (gen, [])

-- Return a function which takes a double between 0 and 1 and returns a value
-- which is more likely to be near the previous value, and guaranteed to not be
-- below or above the min/max.
makeDistribution ::
    -- The previous value
    Int ->
    -- The max. and min. allowable values
    (Int, Int) ->
    -- A double between 0 and 1, which should be chosen uniformly and randomly.
    Double ->
    Int
makeDistribution prevValue (minValue, maxValue) var =
    let leftGradient = 1 / (fromIntegral $ prevValue - minValue)
        rightGradient = 1 / (fromIntegral $ maxValue - prevValue)
        difference = fromIntegral . abs . (prevValue -)
        -- this is perhaps a bit of a misnomer; it doesn't necessarily return a
        -- value x where 0 <= x <= 1
        generateProbability x
            | x < prevValue = 1 - (difference x * leftGradient)
            | x > prevValue = 1 - (difference x * rightGradient)
            | otherwise     = 1
        probabilities = map generateProbability [minValue..maxValue]
        scaleFactor = sum probabilities
        scaledProbs = map (/ scaleFactor) probabilities
        cumulatives = scanl1 (+) scaledProbs
        selectedIndex = length $ takeWhile (<= var) cumulatives
    in  selectedIndex - 1 + minValue

-- makeRoof :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
-- makeRoof (sgen, slice) =
--     let rgen = randomGen sgen
--         th = prevRoofThickness sgen
--         def = sliceDefinition sgen
--         maxDev = maxDeviation def
--         rangeTh = edgeThicknessRange def
--         (rgen', th') = adjustThickness rgen th maxDev rangeTh
--         sgen' = sgen { randomGen = rgen', prevRoofThickness = th' }
--     in (sgen', th' : slice)

-- makeFloor :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
-- makeFloor (sgen, slice) =
--     let rgen = randomGen sgen
--         th = prevFloorThickness sgen
--         def = sliceDefinition sgen
--         maxDev = maxDeviation def
--         rangeTh = edgeThicknessRange def
--         (rgen', th') = adjustThickness rgen th maxDev rangeTh
--         sgen' = sgen { randomGen = rgen', prevFloorThickness = th' }
--         floorDist = (height $ sliceDefinition sgen) - th'
--     in (sgen', floorDist : slice)

-- makeObstacle :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
-- makeObstacle (sgen, slice) = (sgen, slice)
--     -- let slices = slicesToNextObstacle sgen
--     --     makeObstacle' (sgen, slice) =
--     --         let maxObstacleHeight = sliceHeight / 2
--     --             rgen = randomGen sgen
--     --             (rgen', obstacleHeight) = randomR (0, maxObstacleHeight) rgen
--     --             maxObstaclePos = sliceHeight - obstacleHeight
--     --             (rgen'', obstaclePos) = randomR (0, maxObstaclePos) rgen'
--     --             sgen' = sgen
--     --                 { randomGen = rgen'',
--     --                 , slicesToNextObstacle = slicesBetweenObstacles sgen
--     --                 }
--     --         in (sgen', obstacleHeight
--     -- in
--     --     if slices == 0
--     --         then makeObstacle' (sgen, slice)
--     --         else (sgen { slicesToNextObstacle = slices - 1 }, slice)
