module Server.Types where

import Control.Monad.Reader
import Control.Concurrent.STM
import qualified Network.WebSockets as WS
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Data.Map (Map)
import System.Random
import Data.IxSet
import Data.Data (Data, Typeable)
import Web.Scotty.Trans

import Game.Types

-- Scotty things
-- A type for accessing a TVar ServerState inside a Scotty action
newtype WebM a = WebM { runWebM :: ReaderT (TVar ServerState) IO a }
    deriving (Monad, Functor, MonadIO, MonadReader (TVar ServerState))

-- Handy aliases
type Scotty' = ScottyT Text WebM
type Action' = ActionT Text WebM

-- a GameInfo is a game id and an auth token
type GameInfo = (GameId, ByteString)

-- a GameJoinInfo is a GameId, an auth token, and a websocket connection
type GameJoinInfo = (GameId, ByteString, WS.Connection)

-- ServerState
data ServerState = ServerState
    { serverPlayers :: PlayerRepository
    , serverGames   :: GameRepository
    }

-- GameRepository
type Client = (WS.Connection, HeliId)
type Clients = [Client]

data GameRepository = GameRepository
    { repoNextGameId :: GameId
    , repoStdGen     :: StdGen -- for creating Worlds
    , repoGames      :: Map GameId (Game, Clients) -- TODO: Use IntMap
    }

-- PlayerRepository
newtype TokenGenerator = TokenGenerator StdGen deriving (Show)

data PlayerRepository = PlayerRepository
    { repoNextPlayerId   :: PlayerId
    , repoTokenGenerator :: TokenGenerator
    , repoPlayers        :: IxSet Player
    }

-- Player
data Player = Player
    { playerId    :: PlayerId
    , playerName  :: Text
    , playerToken :: ByteString
    }
    deriving (Data, Typeable, Show, Eq, Ord)

newtype PlayerId = PlayerId Int     deriving (Eq, Show, Ord, Data, Typeable, Enum)
newtype Name     = Name Text        deriving (Eq, Show, Ord, Data, Typeable)
newtype Token    = Token ByteString deriving (Eq, Show, Ord, Data, Typeable)
