module Utils where

import System.Random
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Fixed
import Control.Monad

getStdGen' :: IO StdGen
getStdGen' = do
    let time = getCurrentTime
    let timeToInt =
            (floor . (* 1e9) . todSec . localTimeOfDay . utcToLocalTime utc)
    seed <- liftM timeToInt time
    return $ mkStdGen seed
