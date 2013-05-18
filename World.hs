module World where

import System.Random

import Slice
import SliceTest

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

data WorldDef =
    WorldDef
        { dimensions :: Vect Int
        } deriving (Show, Read)

data World =
    World { slices :: [Slice]
          , sliceGen :: StdSliceGen
          , worldDef :: WorldDef
          } deriving (Show, Read)

iterateWorld :: World -> World
iterateWorld = shiftSlices

shiftSlices :: World -> World
shiftSlices wl =
    let (sgen', newSlice) = nextSlice (sliceGen wl)
        sls = slices wl
        sls'
            | length sls == (vectX $ dimensions $ worldDef wl)
                = sls
            | otherwise
                = drop 1 (slices wl) ++ [newSlice]
    in  wl { sliceGen = sgen', slices = sls' }
