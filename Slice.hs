module Slice where

import System.Random

import Utils

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

-- Is an object occupying the vertical space from a to b colliding with any
-- obstacles in a slice?
isSliceClear :: Slice -> Int -> Int -> Bool
isSliceClear s a b =
    isSliceClear' s False
    where
        isSliceClear' slice state =
            case slice of
                []  -> state
                xs  -> recurse xs state
        recurse (x:xs) state
            | x < a     = isSliceClear' xs (not state)
            | x < b     = False
            | otherwise = state

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
        , slicesToNextObstacle = 5
        , randomGen = gen
        , sliceDefinition = def
        }

instance SliceGen StdSliceGen where
    nextSlice gen =
        -- do it back-to-front so that we can build the list up backwards (for
        -- efficiency)
        makeRoof . makeObstacle . makeFloor $ (gen, [])

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

makeRoof :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
makeRoof (sgen, slice) =
    let rgen = randomGen sgen
        th = prevRoofThickness sgen
        def = sliceDefinition sgen
        -- maxDev = maxDeviation def
        rangeTh = edgeThicknessRange def
        (val, rgen') = randomR (0, 1) rgen
        th' = (makeDistribution th rangeTh) val
        sgen' = sgen { randomGen = rgen', prevRoofThickness = th' }
    in (sgen', th' : slice)

makeFloor :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
makeFloor (sgen, slice) =
    let rgen = randomGen sgen
        th = prevFloorThickness sgen
        def = sliceDefinition sgen
        -- maxDev = maxDeviation def
        rangeTh = edgeThicknessRange def
        (val, rgen') = randomR (0, 1) rgen
        th' = (makeDistribution th rangeTh) val
        sgen' = sgen { randomGen = rgen', prevRoofThickness = th' }
        floorDist = (height $ sliceDefinition sgen) - th'
    in (sgen', floorDist : slice)

makeObstacle :: (StdSliceGen, Slice) -> (StdSliceGen, Slice)
makeObstacle (sgen, slice) =
    let countSlices = slicesToNextObstacle sgen
        def = sliceDefinition sgen
        sliceHeight = height def
        makeObstacle' (sgen, slice) =
            let maxObstacleHeight = sliceHeight `div` 2
                rgen = randomGen sgen
                (obstacleHeight, rgen') = randomR (0, maxObstacleHeight) rgen
                maxObstaclePos = sliceHeight - obstacleHeight
                (obstaclePos, rgen'') = randomR (0, maxObstaclePos) rgen'
                sgen' = sgen
                    { randomGen = rgen''
                    , slicesToNextObstacle = slicesBetweenObstacles def
                    }
            in (sgen', obstacleHeight : obstaclePos : slice)
    in if countSlices == 0
        then makeObstacle' (sgen, slice)
        else (sgen { slicesToNextObstacle = countSlices - 1 }, slice)
