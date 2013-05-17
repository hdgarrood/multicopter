module SliceTest where

import Data.List
import Data.Tuple (swap)

import Slice

sliceList :: (SliceGen a) => Int -> a -> [Slice]
sliceList n =
    take n . unfoldr (Just . swap . nextSlice)

getSliceGen :: IO StdSliceGen
getSliceGen =
    let def = SliceDef { edgeThicknessRange = (5, 25)
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
