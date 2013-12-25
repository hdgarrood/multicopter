module Logging where

import Data.Time
import Data.Monoid
import System.IO
import System.Locale

logFile :: FilePath
logFile = "./log/multicopter.log"

formatLogLine :: String -> IO String
formatLogLine msg = do
    time <- getTimeString
    return $ time <> " " <> msg

getTimeString :: IO String
getTimeString =
    fmap (formatTime defaultTimeLocale "[%y-%m-%d %H:%M:%S]") getCurrentTime

putLog :: String -> IO ()
putLog msg = withFile logFile AppendMode $ \h -> formatLogLine msg >>= hPutStrLn h
