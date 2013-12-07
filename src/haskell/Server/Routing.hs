module Server.Routing where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Format as TF

import Game.Types

-- TODO: Do this cleverly, maybe using the same library that Snap uses, so that
-- I don't have to do extra work to get it to go in both directions.
pathForGame :: Game -> Text
pathForGame game = TF.format "/games/{}" (TF.Only $ unGameId $ gameId game)
