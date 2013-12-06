module Server.WebSocketsAppTest where

import Test.HUnit hiding (path)
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

type GetGameIdData = (B.ByteString, Either B.ByteString GameId)

test_getGameId :: GetGameIdData -> Assertion
test_getGameId (path, result) =
    assertEqual' result (getGameId $ mkReq path)

data_getGameId :: [GetGameIdData]
data_getGameId =
    [ ("/ws/games/1"                       , Right $ GameId 1)
    , ("/ws/games/225"                     , Right $ GameId 225)
    , ("/ws/games/23?auth_token=whasdaglw" , Right $ GameId 23)
    , ("/ws/games/not-an-int"              , Left "game not found")
    , ("/this/should/fail"                 , Left "game not found")
    , ("/ws/games/1/this/should/also/fail" , Left "game not found")
    ]

