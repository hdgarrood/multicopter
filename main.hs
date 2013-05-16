module Main where

import System.Random

data Vect = Vect { vectX :: Double
                 , vectY :: Double
                 } deriving (Show, Read)

data Rect = Rect { rectX :: Double
                 , rectY :: Double
                 , rectW :: Double
                 , rectH :: Double
                 } deriving (Show, Read)

-- Do two rectangles overlap?
collisionBetween :: Rect -> Rect -> Bool
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
withinRect :: Vect -> Rect -> Bool
withinRect v r =
    (vectX v) > (rectX r) &&
    (vectX v) < (rectX r + rectW r) &&
    (vectY v) > (rectY r) &&
    (vectY v) < (rectY r + rectW r)

-- A slice of the visible world. Players will spend most of the time across
-- two slices. Has a roof, a floor, and sometimes a third obstacle in the
-- middle.
-- Obstacles are the exact same widths as the slice they occupy.
-- Additionally, they may occupy only one slice.
-- A Slice is represented as a list of Doubles, which are the edges of the
-- contained obstacles. We assume that we're starting inside an obstacle (since
-- we will always have a roof)
type Slice = [Double]

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
                , randomGen :: RandomGen Double
                  -- things that stay the same --
                -- max floor and max roof thickness (they're the same)
                , maxEdgeThickness :: Double
                -- maximum deviation of floor or roof thickness in adjacent
                -- slices
                , maxDeviation :: Double
                -- height of the slice in total
                , sliceHeight :: Double
                }

instance SliceGen StdSliceGen where
    nextSlice gen =
        -- do it back-to-front so that we can build the list up backwards (for
        -- efficiency)
        makeRoof . makeObstacle . makeFloor $ (gen, [])
        where
            -- given a RandomGen Double, a previous thickness, a maximum
            -- deviation, and a maxiumum edge thickness, return a new RandomGen
            -- Double and an adjusted thickness
            -- TODO
            adjustThickness rgen th _ _ = (rgen, th)

            makeRoof (sgen, slice) =
                let rgen = randomGen sgen
                    th = prevRoofThickness sgen
                    maxDev = maxDeviation sgen
                    maxTh = maxEdgeThickness sgen
                    (rgen', th') = adjustThickness rgen th maxDev maxTh
                    sgen' = sgen { randomGen = rgen'
                                 , prevRoofThickness = th'
                                 }
                in (sgen', th' : slice)

            makeFloor (sgen, slice) =
                let rgen = randomGen sgen
                    th = prevFloorThickness sgen
                    maxDev = maxDeviation sgen
                    maxTh = maxEdgeThickness sgen
                    (rgen', th') = adjustThickness rgen th maxDev maxTh
                    sgen' = sgen { randomGen = rgen'
                                 , prevFloorThickness = th'
                                 }
                    floorDist = sliceHeight sgen - th'
                in (sgen', floorDist : slice)

            makeObstacle (sgen, slice) =
                let slices = slicesToNextObstacle sgen
                in
                    if slices == 0
                        then makeObstacle' (sgen, slice)
                        else (sgen { slicesToNextObstacle = slices - 1 }
                             , slice
                             )


data World =
    World { -- The width and height of the World (note that a small portion
            -- on the far right will not be visible)
            dimensions :: Vect,
            -- How quickly obstacles move to the left -- given in units per
            -- iteration
            velocity :: Double,
            -- A list of rectangles which players must avoid crashing into
            obstacles :: [Rect],
            -- An obstacle generator.
            obstacleGen :: ObstacleGen
            -- A random number generator which controls where generated
            -- obstacles appear at the next iteration
            worldGen :: StdGen
          } deriving (Show, Read)

getWidth :: World -> Double
getWidth wl = vectX $ dimensions wl

iterateWorld :: World -> World
iterateWorld = generateNewObstacles . moveObstacles

moveObstacles :: World -> World
moveObstacles wl =
    let os = obstacles wl
        moveObstacle r = r { rectX = rectX r - (velocity wl) }
        os' = map moveObstacle os
    in wl { obstacles = os' }

generateNewObstacles :: World -> World
generateNewObstacles wl =
    if needsNewObstacles wl
        then generateNewObstacles' wl
        else wl
    where
        needsNewObstacles wl =
            let worldWidth = getWidth wl
                furthestRight = worldWidth - 
                difference = width - furthestRight
            in  case (obstacles wl) of
                [] -> False
                _  -> difference > (velocity wl)
        generateNewObstacles' wl = wl
