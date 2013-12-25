module Game.World where

import System.Random
import Control.Monad.Random
import Control.Monad.Writer

import Game.Constants
import Game.Types
import Game.Slice
import Game.Geometry

-- Is a Vect within a Rect?
withinRect :: (Num a, Ord a) => Vect a -> Rect a -> Bool
withinRect v r =
    (vectX v) > (rectX r) &&
    (vectX v) < (rectX r + rectW r) &&
    (vectY v) > (rectY r) &&
    (vectY v) < (rectY r + rectW r)

makeWorld :: Rand StdGen World
makeWorld = do
    gen <- getSplit
    return $ World
        { worldSlices           = replicate c_maxSlicesInWorld c_emptySlice
        , worldSliceGen         = makeSliceGen
        , worldOffset           = 0
        , worldVelocity         = c_startingVelocity
        , worldRandomGen        = gen
        }

iterateWorld :: World -> Writer WorldChanges World
iterateWorld world =
    foldl (>>=) (return world) actions
    where
        actions = [updateSlices, updateOffset]

updateSlices :: World -> Writer WorldChanges World
updateSlices world =
    if needsNewSlice world
        then shiftSlices world
        else return world

needsNewSlice :: World -> Bool
needsNewSlice w =
    floor (worldOffset w) >= c_sliceWidth

shiftSlices :: World -> Writer WorldChanges World
shiftSlices world = do
    let gen     = worldRandomGen world
    let result  = runRand (nextSlice $ worldSliceGen world) gen
    let ((newSlice, sliceGen'), gen') = result

    let sls     = worldSlices world
    -- TODO: choose a data structure with better appending
    let sls'    = drop 1 sls ++ [newSlice]
    let offset' = worldOffset world - (fromIntegral c_sliceWidth)

    tell $ [SliceAdded newSlice]
    return world { worldSliceGen  = sliceGen'
                 , worldSlices    = sls'
                 , worldRandomGen = gen'
                 , worldOffset    = offset'
                 }

updateOffset :: World -> Writer WorldChanges World
updateOffset world = do
    let vel     = worldVelocity world
    let vel'    = vel + c_worldAcceleration
    let offset' = worldOffset world + vel'

    tell $ [SlicesMoved $ round offset']
    return world { worldOffset = offset'
                 , worldVelocity = vel'
                 }

-- If a rectangular object is occupying the space (left edge, right edge) in
-- the World, return the slices that it overlaps
overlappingSlices :: (Int, Int) -> World -> [Slice]
overlappingSlices (left, right) w =
    (map snd) .
    (takeWhile (overlaps . fst)) .
    (dropWhile (not . overlaps . fst)) $
    slicesWithX
    where
        slicesWithX = zip (map (* c_sliceWidth) [0..]) (worldSlices w)
        overlaps sliceX = left < (sliceX + c_sliceWidth) || right > sliceX
