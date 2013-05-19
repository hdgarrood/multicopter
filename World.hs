{-# LANGUAGE OverloadedStrings #-}
module World where

import System.Random
import Control.Monad.Random
import Data.Aeson

import Slice

data Vect a = Vect { vectX :: a
                   , vectY :: a
                   } deriving (Show, Read)

data Rect a = Rect { rectX :: a
                   , rectY :: a
                   , rectW :: a
                   , rectH :: a
                   } deriving (Show, Read)

-- Do two rectangles overlap?
collisionBetween :: (Num a, Ord a) => Rect a -> Rect a -> Bool
collisionBetween a b =
    let ax1 = rectX a
        ax2 = rectX a + rectW a
        ay1 = rectY a
        ay2 = rectY a + rectH a
        bx1 = rectX b
        bx2 = rectX b + rectW b
        by1 = rectY b
        by2 = rectY b + rectH b
    in  (ax1 < bx2) && (ax2 > bx1) && (ay1 < by2) && (ay2 > by1)

-- Is a Vect within a Rect?
withinRect :: (Num a, Ord a) => Vect a -> Rect a -> Bool
withinRect v r =
    (vectX v) > (rectX r) &&
    (vectX v) < (rectX r + rectW r) &&
    (vectY v) > (rectY r) &&
    (vectY v) < (rectY r + rectW r)

-- how many slices can fit into a world at once
worldWidth :: Int
worldWidth = 30

data World =
    World { slices    :: [Slice]
          , sliceGen  :: StdSliceGen
          , randomGen :: StdGen
          } deriving (Show, Read)

makeWorld :: IO World
makeWorld = do
    gen <- getStdGen
    return $ World
        { slices    = replicate worldWidth emptySlice
        , sliceGen  = makeStdSliceGen
        , randomGen = gen
        }

iterateWorld :: World -> World
iterateWorld = shiftSlices

shiftSlices :: World -> World
shiftSlices wl =
    let gen                           = randomGen wl
        ((newSlice, sliceGen'), gen') = runRand (nextSlice $ sliceGen wl) gen
        sls                           = slices wl
        sls'                          = drop 1 sls ++ [newSlice]
    in  wl { sliceGen  = sliceGen'
           , slices    = sls'
           , randomGen = gen'}

-- when sending Worlds back to the clients, they only need to know about the
-- most recent slice
instance ToJSON World where
    toJSON (World slices _ _) = object ["newSlice" .= head slices]
