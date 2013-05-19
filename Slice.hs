module Slice where

import System.Random
import Control.Monad.Random

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

class SliceGen g where
    nextSlice :: g -> Rand StdGen (Slice, g)

-- TODO: Make these parameters of SliceGens?
-- The height of a slice.
sliceHeight :: Int
sliceHeight = 400

-- The mean length of a run of a wall or ceiling at a certain gradient, before
-- another gradient will be chosen.
meanSequenceLength :: Double
meanSequenceLength = 15.0

-- The minimum allowable width of a floor or roof obstacle.
minEdgeWidth :: Int
minEdgeWidth = 10

-- The maximum allowable width of a floor or roof obstacle.
maxEdgeWidth :: Int
maxEdgeWidth = 50

-- The number of slices at the start of a game which will have no central
-- obstacles.
gracePeriod :: Int
gracePeriod = 20

-- we need a single 0 so that the whole slice is empty, rather than full.
emptySlice :: Slice
emptySlice = [0]

-- A StdSliceGen can be thought of as a machine which takes randomness and
-- turns it into slices. Note than no randomness is stored within the
-- StdSliceGen; it must be provided in order for it to produce slices.
data StdSliceGen =
    StdSliceGen
        { roofWidthGen          :: EdgeWidthGen
        , floorWidthGen         :: EdgeWidthGen
        , slicesToNextObstacle  :: Int
        } deriving (Show, Read)

makeStdSliceGen :: StdSliceGen
makeStdSliceGen =
    StdSliceGen
        { roofWidthGen         = makeEdgeWidthGen
        , floorWidthGen        = makeEdgeWidthGen
        , slicesToNextObstacle = gracePeriod
        }

data EdgeWidthGen =
    EdgeWidthGen
        { remainingSlicesInSequence :: Int
        , currentGradient           :: Int
        , currentWidth              :: Int
        } deriving (Show, Read)

makeEdgeWidthGen :: EdgeWidthGen
makeEdgeWidthGen =
    EdgeWidthGen
        { remainingSlicesInSequence = 0
        , currentGradient           = 0
        , currentWidth              = (maxEdgeWidth + minEdgeWidth) `div` 2
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
    param <- getRandomR (0, 1) :: Rand StdGen Double
    let newLength     =  makePoisson meanSequenceLength $ param
    let width         =  currentWidth gen
    let gradientRange =  ((minEdgeWidth - width) `div` newLength,
                          (maxEdgeWidth - width) `div` newLength)

    newGradient       <- getRandomR gradientRange
    let gen'          =  gen { remainingSlicesInSequence = newLength
                             , currentGradient           = newGradient
                             }
    continueEdgeSequence gen'

instance SliceGen StdSliceGen where
    nextSlice gen = do
        let roofGen             =  roofWidthGen gen
        (roofWidth, roofGen')   <- nextEdgeWidth roofGen

        let floorGen            =  floorWidthGen gen
        (floorWidth, floorGen') <- nextEdgeWidth floorGen
        let floorWidth'         =  sliceHeight - floorWidth

        let gen'                =  gen { roofWidthGen = roofGen',
                                         floorWidthGen = floorGen' }

        return ([roofWidth, floorWidth'], gen')

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
        probs = map pmf [0..maxK]
        cumulativeProbs = scanl1 (+) probs
    in  cumulativeProbs
