module Game.Heli where

import Data.Text.Lazy (Text)
import Control.Monad.Writer
import Data.Maybe

import Game.Types
import Game.World
import Game.Slice
import Game.Constants

makeHeli :: HeliId -> Text -> Heli
makeHeli hId name = Heli
    { heliId        = hId
    , heliName      = name
    , heliPosition  = 0
    , heliVelocity  = 0
    , heliDirection = Down
    , heliIsAlive   = True
    }

shouldStartGame :: HeliInputData -> Bool
shouldStartGame = any f
    where
        f StartTheGame = True
        f _            = False

getNewDirection :: HeliInputData -> Maybe Direction
getNewDirection = foldr f Nothing
    where
        f (ChangeDirection x) _ = Just x
        f _ v = v

handleEvents :: HeliInputData -> Game -> Heli -> Writer HeliEvents ()
handleEvents input game heli =
    when (heliIsAlive heli) $ do
        -- change direction
        let newDir = getNewDirection input
        when (isJust newDir) (tell [ChangedDirection (fromJust newDir)])

        -- handle wall collisions
        let world = gameWorld game
        unless (isWorldClear world heli) (tell [CollidedWithWall])

        -- TODO: handle player collisions

isWorldClear :: World -> Heli -> Bool
isWorldClear w h = check slices
    where
        check  = all (\s -> isSliceClear s top bottom)
        top    = heliPosition h
        bottom = top + c_heliHeight

        slices = overlappingSlices (left, right) w
        left   = c_heliXPosition
        right  = left + c_heliWidth

doLogic :: Heli -> HeliEvents -> Writer [HeliChange] Heli
doLogic h _ = return h
