module Server.GameRepository where

import qualified Data.Map as M
import Control.Monad.Random

import Game.Types
import Server.Types
import Game.Game

noClients :: Clients
noClients = []

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
                     , repoGames      = M.insert gId
                                            (game, noClients)
                                            (repoGames repo)
                     }

getGameById :: GameId -> GameRepository -> Maybe (Game, Clients)
getGameById gId repo =
    M.lookup gId (repoGames repo)
