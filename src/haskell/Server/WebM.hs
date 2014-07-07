module Server.WebM where

import Web.Scotty.Trans
import Control.Concurrent.STM
import Control.Monad.Reader
import Network.Wai (Application)

import Server.Types

scottyAppWebM :: TVar ServerState -> Scotty' () -> IO Application
scottyAppWebM tvar app = scottyAppT (unWebM tvar) (unWebM tvar) app

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (ServerState -> a) -> WebM a
gets f = do
    var <- ask
    val <- liftIO (readTVarIO var)
    return (f val)

modify :: (ServerState -> ServerState) -> WebM ()
modify f = do
    var <- ask
    liftIO $ atomically $ modifyTVar' var f

modifyWith :: (ServerState -> (a, ServerState)) -> WebM a
modifyWith f = do
    var <- ask
    liftIO $ atomically $ do
        value <- readTVar var
        let (a, result) = f value
        writeTVar var result
        return a

modifyPlayersWith :: (PlayerRepository -> (a, PlayerRepository)) -> WebM a
modifyPlayersWith f = modifyWith f'
    where
        f' ss =
            let (x, pr') = f (serverPlayers ss)
            in  (x, ss { serverPlayers = pr' })

modifyGamesWith :: (GameRepository -> (a, GameRepository)) -> WebM a
modifyGamesWith f = modifyWith f'
    where
        f' ss =
            let (x, gr') = f (serverGames ss)
            in  (x, ss { serverGames = gr' })
