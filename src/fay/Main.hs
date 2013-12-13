module Main where

import Prelude
import FFI

import Game.Constants

type Slice = [Int]

emptySlice :: Slice
emptySlice = [0]

data World = World
    { worldSlices :: [Slice]
    , worldOffset :: Double 
    }

initialWorld :: World
initialWorld = World
    { worldSlices = replicate maxSlicesInWorld emptySlice
    , worldOffset = 0
    }

getLocationHost :: Fay String
getLocationHost = ffi "window.location.host"

constructWebsocketUrl :: String -> String -> Fay String
constructWebsocketUrl path authToken = do
    hostname <- getLocationHost
    return $ concat $
        [ "ws://"
        , hostname
        , path
        , "?auth_token="
        , authToken
        ]

main :: Fay ()
main = return ()

-- $(document).ready(function() {"
--   var websocket_url = ["
--     'ws://',"
--     window.location.host,"
-- F.format "'{}'," (Only (wsPathForGame game))
--     '?auth_token=',"
--     $.cookie('auth_token'),"
--   ].join('')"
--   startGame(websocket_url)"
-- })"
