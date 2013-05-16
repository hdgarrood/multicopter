module SliceTest where

import System.Random
import System.IO.Unsafe
import Data.List
import Data.Tuple (swap)
import Slice

sliceList :: (SliceGen a) => Int -> a -> [Slice]
sliceList n =
    take n . unfoldr (Just . swap . nextSlice)

exampleSliceGen :: StdSliceGen
exampleSliceGen = StdSliceGen
    { prevRoofThickness = 0
    , prevFloorThickness = 0
    , slicesToNextObstacle = 5
    , randomGen = unsafePerformIO getStdGen
    , maxEdgeThickness = 0.1
    , maxDeviation = 0.05
    , slicesBetweenObstacles = 5
    }
    
showSlice :: Slice -> String
showSlice slice = "|" ++ showSlice' scaled 'x' ++ "|"
    where
        scaleFactor = 80
        scaled = map (floor . (* scaleFactor)) slice
        flip ch =
            case ch of
                ' ' -> 'x'
                'x' -> ' '
                _   -> error "wtf"
        showSlice' slice ch =
            case slice of
                []      -> ""
                (x:xs)  ->
                    replicate x ch ++ showSlice' xs (flip ch)
