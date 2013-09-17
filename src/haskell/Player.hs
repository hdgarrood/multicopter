module Player where

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,
GeneralizedNewTypeDeriving #-}

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import Data.IxSet
import Data.Data                 (Data, Typeable)
import System.Random

newtype PlayerId = PlayerId Int     deriving (Eq, Show, Ord, Data, Typeable, Enum)
newtype Name     = Name ByteString  deriving (Eq, Show, Ord, Data, Typeable)
newtype Token    = Token ByteString deriving (Eq, Show, Ord, Data, Typeable)

data Player = Player
    { playerId :: PlayerId
    , name     :: ByteString
    , token    :: ByteString
    }
    deriving (Data, Typeable, Show, Eq, Ord)

data PlayerRepository = PlayerRepository
    { nextPlayerId :: PlayerId
    , players      :: IxSet Player
    }
    deriving (Data, Typeable)

instance Indexable Player where
    empty = ixSet
                [ ixFun $ \p -> [ playerId p ]
                , ixFun $ \p -> [ Token $ token p ]
                ]

initialPlayerRepository :: PlayerRepository
initialPlayerRepository =
    PlayerRepository
        { nextPlayerId = PlayerId 1
        , players      = empty
        }

-- Add a player to the repository. Returns the newly added player and the
-- repository.
addPlayer :: PlayerRepository -> ByteString -> (Player, PlayerRepository)
addPlayer repo playerName =
    let thisId = nextPlayerId repo
        player = Player
                    { playerId = thisId
                    , name     = playerName
                    , token    = "" -- TODO: generate these
                    }
        repo' = repo
                    { nextPlayerId = succ thisId
                    , players      = insert player $ players repo
                    }
    in (player, repo')

-- Get a player by ID.
getPlayerById :: PlayerRepository -> PlayerId -> Maybe Player
getPlayerById repo pid =
    getOne $ players repo @= pid
