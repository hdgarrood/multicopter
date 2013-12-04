module Server.GameRepository where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Random

import Game.Types
import Game.Game

data GameRepository = GameRepository
    { repoNextGameId :: GameId
    , repoStdGen     :: StdGen -- for creating Worlds
    , repoGames      :: Map GameId Game
    }

makeGameRepository :: Rand StdGen GameRepository
makeGameRepository = do
    gen <- getSplit
    return $
        GameRepository { repoNextGameId = GameId 0
                       , repoStdGen     = gen
                       , repoGames      = M.empty
                       }

addGame :: GameRepository -> (Game, GameRepository)
addGame repo = (game, repo')
    where
        gId          = repoNextGameId repo
        (game, gen') = runRand (makeGame gId) (repoStdGen repo)

        repo' = repo { repoNextGameId = succ gId
                     , repoStdGen     = gen'
                     , repoGames      = M.insert gId game (repoGames repo)
                     }

getGameById :: GameId -> GameRepository -> Maybe Game
getGameById gId repo =
    M.lookup gId (repoGames repo)
