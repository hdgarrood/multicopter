module Game.World where

import System.Random
import Control.Monad.Random
import Control.Monad.Writer

import qualified Data.Text as T
import Data.Aeson

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

makeWorld :: IO World
makeWorld = do
    gen <- getStdGen
    return $ World
        { slices           = replicate maxSlicesInWorld emptySlice
        , sliceGen         = makeStdSliceGen
        , offset           = 0
        , velocity         = startingVelocity
        , randomGen        = gen
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
    floor (offset w) >= sliceWidth

shiftSlices :: World -> Writer WorldChanges World
shiftSlices world = do
    let gen     = randomGen world
    let ((newSlice, sliceGen'), gen') = runRand
                                            (nextSlice $ sliceGen world) gen
    let sls     = slices world
    let sls'    = drop 1 sls ++ [newSlice]
    let offset' = offset world - (fromIntegral sliceWidth)

    tell $ [SliceAdded newSlice]
    return world { sliceGen  = sliceGen'
                 , slices    = sls'
                 , randomGen = gen'
                 , offset    = offset'
                 }

updateOffset :: World -> Writer WorldChanges World
updateOffset world = do
    let vel     = velocity world
    let vel'    = vel + worldAcceleration
    let offset' = offset world + vel'

    tell $ [SlicesMoved $ round offset']
    return world { offset = offset'
                 , velocity = vel'
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
        slicesWithX = zip (map (* sliceWidth) [0..]) (slices w)
        overlaps sliceX = left < (sliceX + sliceWidth) || right > sliceX
