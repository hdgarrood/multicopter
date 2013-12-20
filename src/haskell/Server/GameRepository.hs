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
        GameRepository { repoNextGameId = GameId 1
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

getAllGames :: GameRepository -> [(Game, Clients)]
getAllGames = M.elems . repoGames

getAllGames' :: GameRepository -> [Game]
getAllGames' = map fst . getAllGames

modifyGame :: GameId ->
              (Game, Clients) ->
              GameRepository ->
              Maybe GameRepository
modifyGame gameKey gameVal repo =
    if M.member gameKey (repoGames repo)
        then let games = M.insert gameKey gameVal (repoGames repo)
             in  Just $ repo { repoGames = games }
        else Nothing

reconstructGames :: [(Game, Clients)] -> GameRepository -> GameRepository
reconstructGames games repo =
    repo { repoGames = makeGameMap games }
    where
        makeGameMap = foldr add M.empty
        add (g, cs) = M.insert (gameId g) (g, cs)
