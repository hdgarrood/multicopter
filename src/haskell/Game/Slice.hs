module Game.Slice where

import System.Random
import Control.Monad.Random

import Game.Types
import Game.Constants

-- Is an object occupying the vertical space from a to b colliding with any
-- obstacles in a slice?
isSliceClear :: Slice -> Int -> Int -> Bool
isSliceClear s a b =
    isSliceClear' s False
    where
        isSliceClear' []     state = state
        isSliceClear' (x:xs) state
            | x < a     = isSliceClear' xs (not state)
            | x < b     = False
            | otherwise = state

makeSliceGen :: SliceGen
makeSliceGen =
    SliceGen
        { roofWidthGen  = makeEdgeWidthGen
        , floorWidthGen = makeEdgeWidthGen
        , obstacleGen   = makeObstacleGen
        }

makeEdgeWidthGen :: EdgeWidthGen
makeEdgeWidthGen =
    EdgeWidthGen
        { remainingSlicesInSequence = 0
        , currentGradient           = 0
        , currentWidth              = (maxEdgeWidth + minEdgeWidth) `div` 2
        }

makeObstacleGen :: ObstacleGen
makeObstacleGen =
    ObstacleGen
        { slicesUntilNextObstacle = gracePeriod
        }

nextEdgeWidth :: EdgeWidthGen -> Rand StdGen (Int, EdgeWidthGen)
nextEdgeWidth gen
    | slices > 0  = continueEdgeSequence gen
    | otherwise   = startNewEdgeSequence gen
    where
        slices = remainingSlicesInSequence gen

continueEdgeSequence :: EdgeWidthGen -> Rand StdGen (Int, EdgeWidthGen)
continueEdgeSequence gen =
    let slices = remainingSlicesInSequence gen
        nextWidth = (currentWidth gen) + (currentGradient gen)
        gen' = gen { currentWidth              = nextWidth
                   , remainingSlicesInSequence = slices - 1
                   }
    in return $ (nextWidth, gen')

startNewEdgeSequence :: EdgeWidthGen -> Rand StdGen (Int, EdgeWidthGen)
startNewEdgeSequence gen = do
    param             <- getRandomR (0, 1) :: Rand StdGen Double
    let newLength     =  makePoisson meanSequenceLength $ param
    let width         =  currentWidth gen
    let gradientRange =  ((minEdgeWidth - width) `div` newLength,
                          (maxEdgeWidth - width) `div` newLength)

    newGradient       <- getRandomR gradientRange
    let gen'          =  gen { remainingSlicesInSequence = newLength
                             , currentGradient           = newGradient
                             }
    continueEdgeSequence gen'

-- Given a slice, which should already have the top and bottom obstacles,
-- return a new slice (and an updated ObstacleGen) which may have had an
-- obstacle inserted.
--
-- Obstacles are added at a constant overall rate, but distributed randomly, by
-- means of a Poisson distribution.
addObstacle :: ObstacleGen -> Slice -> Rand StdGen (Slice, ObstacleGen)
addObstacle oGen slice
    | slices > 0 = continueObstacleSequence oGen slice
    | otherwise  = startNewObstacleSequence oGen slice
    where
        slices = slicesUntilNextObstacle oGen

continueObstacleSequence ::
    ObstacleGen -> Slice -> Rand StdGen (Slice, ObstacleGen)
continueObstacleSequence gen slice =
    let slices = slicesUntilNextObstacle gen
        gen'   = gen { slicesUntilNextObstacle = slices - 1 }
    in  return $ (slice, gen')

startNewObstacleSequence ::
    ObstacleGen -> Slice -> Rand StdGen (Slice, ObstacleGen)
startNewObstacleSequence gen slice = do
    let cdf             =  makePoisson meanSlicesBetweenObstacles
    param               <- getRandomR (0, 1) :: Rand StdGen Double
    let slicesUntilNext =  cdf param

    obstacleWidth       <- getRandomR (minObstacleWidth, maxObstacleWidth)

    let roofW           =  slice !! 0
    let floorW          =  slice !! 1 -- to avoid shadowing Prelude.floor
    obstacleOffset      <- getRandomR (roofW, floorW - obstacleWidth)

    let gen'            =  gen { slicesUntilNextObstacle = slicesUntilNext
                               }
    return $
        ( [roofW, obstacleOffset, obstacleOffset + obstacleWidth, floorW]
        , gen'
        )

nextSlice :: SliceGen -> Rand StdGen (Slice, SliceGen)
nextSlice gen = do
    -- Generate the floor
    let floorGen            =  floorWidthGen gen
    (floorWidth, floorGen') <- nextEdgeWidth floorGen
    let floorWidth'         =  sliceHeight - floorWidth
    let slice               =  [floorWidth']

    -- Generate the roof
    let roofGen             =  roofWidthGen gen
    (roofWidth, roofGen')   <- nextEdgeWidth roofGen
    let slice'              =  roofWidth : slice


    -- Maybe generate an obstacle
    let oGen                =  obstacleGen gen
    (slice'', oGen')        <- addObstacle oGen slice'

    -- Update the SliceGen
    let gen'                =  gen { roofWidthGen  = roofGen'
                                   , obstacleGen   = oGen'
                                   , floorWidthGen = floorGen'
                                   }

    return (slice'', gen')

-- Given a value for lambda, make a cumulative poission distribution function.
makePoisson :: Double -> Double -> Int
makePoisson lambda =
    let cumulativeProbs = makePoissonCumulativeProbs lambda
    in  \x -> length $ takeWhile (< x) cumulativeProbs

-- Take a mean, and return the cumulative probabilities of a Poisson
-- distribution with that mean.
makePoissonCumulativeProbs :: Double -> [Double]
makePoissonCumulativeProbs lambda =
    let fac n = product [1..n]
        pmf k = (lambda ^ k) * (exp (negate lambda)) / (fromIntegral $ fac k)
        maxK = floor $ 3 * lambda
        probs = map pmf [0..maxK::Int]
        cumulativeProbs = scanl1 (+) probs
    in  cumulativeProbs
