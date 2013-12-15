{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Websocket where

import Prelude
import FFI
import Fay.Text (Text)
import qualified Fay.Text as T
--import Data.Map

data WebSocket
data WebSocketMessageEvent

getData :: WebSocketMessageEvent -> Fay Text
getData = ffi "%1.data"

type Callback = WebSocketMessageEvent -> Fay ()

data Callbacks = Callbacks { onOpen    :: Maybe Callback
                           , onMessage :: Maybe Callback
                           , onClose   :: Maybe Callback
                           }

defaultCallbacks :: Callbacks
defaultCallbacks = Callbacks { onOpen    = Nothing
                             , onMessage = Nothing
                             , onClose   = Nothing
                             }

debugCallbacks :: Callbacks
debugCallbacks = Callbacks { onOpen    = cb "open"
                           , onMessage = cb "message"
                           , onClose   = cb "close"
                           }
    where
        cb evName = Just $ \event -> do
            msg <- getData event
            putStrLn $ evName ++ ": " ++ (show msg)

callbackFor :: Text -> Callbacks -> Maybe Callback
callbackFor "open"    = onOpen
callbackFor "message" = onMessage
callbackFor "close"   = onClose
callbackFor _         = const Nothing

createWebSocket :: Text -> Fay WebSocket
createWebSocket = ffi "new WebSocket(%1)"

addEventListener :: WebSocket -> Text -> Callback -> Fay ()
addEventListener = ffi "%1.addEventListener(%2, %3)"

whenJust :: Maybe a -> (a -> Fay ()) -> Fay ()
whenJust (Just a) f = f a
whenJust Nothing _  = return ()

open :: Text -> Callbacks -> Fay WebSocket
open url callbacks = do
    websocket <- createWebSocket url
    forM_ ["open", "message", "close"] $ \ev -> do
        let callback = callbackFor ev callbacks
        whenJust callback (addEventListener websocket ev)
    return websocket

send :: WebSocket -> Text -> Fay ()
send = ffi "%1.send(%2)"
