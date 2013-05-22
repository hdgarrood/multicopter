{-# LANGUAGE OverloadedStrings #-}
module World where

import System.Random
import Control.Monad.Random
import Control.Monad.Writer

import qualified Data.Text as T
import Data.Aeson

import Slice
import Geometry

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
startingVelocity = 3.0

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

-- Any piece of information which we send back to clients regarding a change of
-- state in the world is packaged up as a WorldChange.
data WorldChange = SliceAdded Slice -- A slice was added
                 | SlicesMoved Double -- Tells how far the slices moved
                 | PlayerMoved (Vect Double) -- Gives new player positions
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
    toJSON (PlayerMoved vec) =
        object [ "type" .= ("playerMoved" :: T.Text)
               , "data" .= vec
               ]

type WorldChanges = [WorldChange]

iterateWorld :: World -> Writer WorldChanges World
iterateWorld world =
    return world 
        >>= updateOffset
        >>= updateSlices

updateSlices :: World -> Writer WorldChanges World
updateSlices world =
    if needsNewSlice world
        then shiftSlices world
        else return world

needsNewSlice :: World -> Bool
needsNewSlice (World _ _ offset _ _ ) =
    floor offset >= sliceWidth

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
    let offset' = offset world + vel

    tell $ [SlicesMoved offset']
    return world { offset = offset' }
