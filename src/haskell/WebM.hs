module WebM where

import Control.Concurrent.STM
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Reader

import ServerState

newtype WebM a = WebM { runWebM :: ReaderT (TVar ServerState) IO a }
    deriving (Monad, MonadIO, MonadReader (TVar ServerState))

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
