{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Prelude
import Fay.Text (Text, fromString)
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

getAttr' :: Text -> JQuery -> Fay (Defined Text)
getAttr' = ffi "%2['attr'](%1)"

getWebSocketPath :: Fay (Defined Text)
getWebSocketPath = do
    elem <- select "#canvas-container"
    getAttr' "data-websocket-path" elem

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing  = Nothing

constructWebSocketUrl :: Fay Text
constructWebSocketUrl = do
    hostname <- getHostname
    Defined path <- getWebSocketPath
    return $ "ws://" `T.append` hostname `T.append` path

alert :: Text -> Fay ()
alert = ffi "window.alert(%1)"

main :: Fay ()
main = do
    url <- constructWebSocketUrl
    ws <- W.open url W.debugCallbacks
    W.addEventListener ws "open" $ (\_ -> W.send ws "hello")
