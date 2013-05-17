module SliceTest where

import Data.List
import Data.Tuple (swap)

import Slice hiding (isSliceClear)

sliceList :: (SliceGen a) => Int -> a -> [Slice]
sliceList n =
    take n . unfoldr (Just . swap . nextSlice)

getSliceGen :: IO StdSliceGen
getSliceGen =
    let def = SliceDef { edgeThicknessRange = (3, 3)
                       , maxDeviation = 1
                       , slicesBetweenObstacles = 5
                       , height = 150
                       }
    in mkStdSliceGen def

showSlice :: Slice -> String
showSlice slice = "|" ++ sliceStr ++ "|"
    where
        sliceStr = map (\x ->
            if isSliceClear slice x x
                then ' '
                else 'I') [1..150]

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
