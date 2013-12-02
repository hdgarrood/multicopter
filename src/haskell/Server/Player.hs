module Server.Player where

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.IxSet
import           Data.Data (Data, Typeable)
import           Data.Text.Lazy (Text)
import           System.Random

import           Server.TokenGenerator

newtype PlayerId = PlayerId Int     deriving (Eq, Show, Ord, Data, Typeable, Enum)
newtype Name     = Name Text        deriving (Eq, Show, Ord, Data, Typeable)
newtype Token    = Token ByteString deriving (Eq, Show, Ord, Data, Typeable)

unPlayerId :: PlayerId -> Int
unPlayerId (PlayerId x) = x

data Player = Player
    { playerId    :: PlayerId
    , playerName  :: Text
    , playerToken :: ByteString
    }
    deriving (Data, Typeable, Show, Eq, Ord)

data PlayerRepository = PlayerRepository
    { repoNextPlayerId   :: PlayerId
    , repoTokenGenerator :: TokenGenerator
    , repoPlayers        :: IxSet Player
    }

instance Indexable Player where
    empty = ixSet
                [ ixFun $ \p -> [ playerId p ]
                , ixFun $ \p -> [ Token $ playerToken p ]
                ]

makePlayerRepository :: IO PlayerRepository
makePlayerRepository = do
    tokGen <- getTokenGenerator
    return $ PlayerRepository
        { repoNextPlayerId   = PlayerId 1
        , repoTokenGenerator = tokGen
        , repoPlayers        = empty
        }

-- Add a player to the repository. Returns the newly added player and the
-- repository.
addPlayer :: Text -> PlayerRepository -> (Player, PlayerRepository)
addPlayer playerName repo =
    let thisId = repoNextPlayerId repo
        tokGen = repoTokenGenerator repo
        (tok, tokGen') = nextToken tokGen
        player = Player
                    { playerId = thisId
                    , playerName     = playerName
                    , playerToken    = tok
                    }
        repo' = repo
                    { repoNextPlayerId   = succ thisId
                    , repoTokenGenerator = tokGen'
                    , repoPlayers        = insert player $ repoPlayers repo
                    }
    in (player, repo')

-- Get a player by ID.
getPlayerById :: PlayerId -> PlayerRepository -> Maybe Player
getPlayerById pid repo =
    getOne $ repoPlayers repo @= pid

-- Get a player by token.
getPlayerByToken :: Token -> PlayerRepository -> Maybe Player
getPlayerByToken tok repo =
    getOne $ repoPlayers repo @= tok

getAllPlayers :: PlayerRepository -> [Player]
getAllPlayers repo =
    toList $ repoPlayers repo
