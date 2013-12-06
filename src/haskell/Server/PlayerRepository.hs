module Server.PlayerRepository where

import Data.IxSet
import Data.Data (Typeable)
import Data.Text.Lazy (Text)
import Data.Maybe (isJust)
import Control.Monad.Random

import Server.Types
import Server.Player()
import Server.TokenGenerator

makePlayerRepository :: Rand StdGen PlayerRepository
makePlayerRepository = do
    tokGen <- getTokenGenerator
    return $ PlayerRepository
        { repoNextPlayerId   = PlayerId 1
        , repoTokenGenerator = tokGen
        , repoPlayers        = empty
        }

-- Add a player to the repository. Returns the newly added player (or an error
-- Text if something went wrong) and the perhaps-changed repository.
addPlayer :: Text ->
             PlayerRepository ->
             (Either Text Player, PlayerRepository)
addPlayer name repo =
    if playerExistsWith (Name name) repo
        then (Left "There is already a player with that name.", repo)
        else let (p, repo') = addPlayerUnsafe name repo
             in (Right p, repo')

-- Adds a player to the repository, regardless of whether this should be
-- allowed.
addPlayerUnsafe :: Text -> PlayerRepository -> (Player, PlayerRepository)
addPlayerUnsafe name repo =
    let thisId = repoNextPlayerId repo
        tokGen = repoTokenGenerator repo
        (tok, tokGen') = nextToken tokGen
        player = Player
                    { playerId    = thisId
                    , playerName  = name
                    , playerToken = tok
                    }
        repo' = repo
                    { repoNextPlayerId   = succ thisId
                    , repoTokenGenerator = tokGen'
                    , repoPlayers        = insert player $ repoPlayers repo
                    }
    in (player, repo')

class Typeable a => PlayerIx a where
    getPlayerBy :: a -> PlayerRepository -> Maybe Player
    getPlayerBy key repo =
        getOne $ repoPlayers repo @= key

    playerExistsWith :: a -> PlayerRepository -> Bool
    playerExistsWith key repo = isJust $ getPlayerBy key repo

instance PlayerIx PlayerId
instance PlayerIx Name
instance PlayerIx Token

getAllPlayers :: PlayerRepository -> [Player]
getAllPlayers repo =
    toList $ repoPlayers repo
