{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Fay.Text (Text)
import qualified Fay.Text as T
import JQuery
import FFI

import qualified Websocket as W
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

getHostname :: Fay Text
getHostname = ffi "window.location.host"

getWebSocketPath :: Fay Text
getWebSocketPath = do
    elem <- select ("#canvas-container" :: Text)
    getAttr "data-websocket-path" elem

getCookieUnparsed :: Fay Text
getCookieUnparsed = ffi "document.cookie"

type Cookies = [(Text, Text)]

getCookie :: Fay (Maybe Cookies)
getCookie = do
    cookie <- getCookieUnparsed
    return $ parseCookie cookie

parseCookie :: Text -> Maybe Cookies
parseCookie _ = Nothing

constructWebSocketUrl :: Text -> Fay Text
constructWebSocketUrl authToken = do
    hostname <- getHostname
    path <- getWebSocketPath
    return $ T.concat $
        [ "ws://"
        , hostname
        , path
        , "?auth_token="
        , authToken
        ]

alert :: Text -> Fay ()
alert = ffi "window.alert(%1)"

main :: Fay ()
main = do
    ws <- W.open "ws://echo.websocket.org" W.debugCallbacks
    W.addEventListener ws "open" $ (\_ -> W.send ws "hello")
