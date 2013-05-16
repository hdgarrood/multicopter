module SliceTest where

import System.Random
import System.IO.Unsafe
import Data.List
import Data.Tuple (swap)
import Slice hiding (isSliceClear)

sliceList :: (SliceGen a) => Int -> a -> [Slice]
sliceList n =
    take n . unfoldr (Just . swap . nextSlice)

getSliceGen :: IO StdSliceGen
getSliceGen = do
    gen <- getStdGen
    return StdSliceGen
        { prevRoofThickness = 0
        , prevFloorThickness = 0
        , slicesToNextObstacle = 5
        , randomGen = gen
        , edgeThicknessRange = (0.1, 0.3)
        , maxDeviation = 0.025
        , slicesBetweenObstacles = 5
        }

showSlice :: Slice -> String
showSlice slice = "|" ++ sliceStr ++ "|"
    where
        scaleFactor = 80
        scaled = map (floor . (* scaleFactor)) slice
        flip ch =
            case ch of
                ' ' -> 'x'
                'x' -> ' '
                _   -> error "wtf"
        sliceStr = map (\x ->
            if isSliceClear scaled x x
                then ' '
                else 'x') [1..80]

showSlices :: [Slice] -> String
showSlices =
     unlines . map showSlice

sliceTest :: IO ()
sliceTest = do
    sg <- getSliceGen
    let slices = sliceList 20 sg
    mapM_ (putStrLn . showSlice) slices

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
