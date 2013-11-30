module Player where

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.IxSet
import           Data.Data (Data, Typeable)
import           System.Random

import           TokenGenerator

newtype PlayerId = PlayerId Int     deriving (Eq, Show, Ord, Data, Typeable, Enum)
newtype Name     = Name ByteString  deriving (Eq, Show, Ord, Data, Typeable)
newtype Token    = Token ByteString deriving (Eq, Show, Ord, Data, Typeable)

unPlayerId :: PlayerId -> Int
unPlayerId (PlayerId x) = x

data Player = Player
    { playerId :: PlayerId
    , name     :: ByteString
    , token    :: ByteString
    }
    deriving (Data, Typeable, Show, Eq, Ord)

data PlayerRepository = PlayerRepository
    { nextPlayerId   :: PlayerId
    , tokenGenerator :: TokenGenerator
    , players        :: IxSet Player
    }

instance Indexable Player where
    empty = ixSet
                [ ixFun $ \p -> [ playerId p ]
                , ixFun $ \p -> [ Token $ token p ]
                ]

makePlayerRepository :: IO PlayerRepository
makePlayerRepository = do
    tokGen <- getTokenGenerator
    return $ PlayerRepository
        { nextPlayerId   = PlayerId 1
        , tokenGenerator = tokGen
        , players        = empty
        }

-- Add a player to the repository. Returns the newly added player and the
-- repository.
addPlayer :: ByteString -> PlayerRepository -> (Player, PlayerRepository)
addPlayer playerName repo =
    let thisId = nextPlayerId repo
        tokGen = tokenGenerator repo
        (tok, tokGen') = nextToken tokGen
        player = Player
                    { playerId = thisId
                    , name     = playerName
                    , token    = tok
                    }
        repo' = repo
                    { nextPlayerId   = succ thisId
                    , tokenGenerator = tokGen'
                    , players        = insert player $ players repo
                    }
    in (player, repo')

-- Get a player by ID.
getPlayerById :: PlayerId -> PlayerRepository -> Maybe Player
getPlayerById pid repo =
    getOne $ players repo @= pid

-- Get a player by token.
getPlayerByToken :: Token -> PlayerRepository -> Maybe Player
getPlayerByToken tok repo =
    getOne $ players repo @= tok

getAllPlayers :: PlayerRepository -> [Player]
getAllPlayers repo =
    toList $ players repo
