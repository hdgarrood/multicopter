module Server.WebSocketsAppTest where

import Test.HUnit
import qualified Data.ByteString as B
import qualified Network.WebSockets as WS

import Server.WebSocketsApp
import Game.Types

-- helpers
--
-- Make a WS.RequestHead just from the path info
mkReq :: B.ByteString -> WS.RequestHead
mkReq pathInfo = WS.RequestHead pathInfo [] False

-- for brevity
assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

test_getGameId :: Assertion
test_getGameId =
    assertEqual'
        (Right $ GameId 1)
        (getGameId $ mkReq "/ws/games/1?auth_token=foo")
