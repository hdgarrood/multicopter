module Server.WebM where

import Web.Scotty.Trans
import Control.Concurrent.STM
import Control.Monad.Reader

import Server.ServerState

newtype WebM a = WebM { runWebM :: ReaderT (TVar ServerState) IO a }
    deriving (Monad, Functor, MonadIO, MonadReader (TVar ServerState))

scottyWebM :: Int -> TVar ServerState -> ScottyT WebM () -> IO ()
scottyWebM port tvar app =
    scottyT port runM runActionToIO app
    where
        runM m = runReaderT (runWebM m) tvar
        runActionToIO = runM

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
