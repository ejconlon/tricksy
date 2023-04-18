{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Tricksy.Monad
  ( SpawnError (..)
  , ResM
  , runResM
  , allocate
  , allocate_
  , register
  , scoped
  , spawnThread
  , spawnAsync
  , stopThreads
  , stopThread
  )
where

import Control.Concurrent (ThreadId, forkIO, throwTo)
import Control.Concurrent.Async (Async (..), AsyncCancelled (..), async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar, tryPutTMVar, tryReadTMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (AsyncException, SomeAsyncException, SomeException, catch, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (Exception (fromException), MonadCatch, MonadMask (..), MonadThrow, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT (..), ask, asks)
import Control.Monad.Trans.Resource (ResIO)
import Control.Monad.Trans.Resource qualified as R
import Data.Foldable (for_)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set

newtype SpawnError = SpawnError {unSpawnError :: SomeException}
  deriving stock (Show)

instance Exception SpawnError

data ResEnv = ResEnv
  { reThreadIdsVar :: !(TVar (Set ThreadId))
  , reErrVar :: !(TMVar SpawnError)
  }

newResEnv :: IO ResEnv
newResEnv = ResEnv <$> newTVarIO Set.empty <*> newEmptyTMVarIO

newtype ResM a = ResM {unResM :: ReaderT ResEnv ResIO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadUnliftIO
    , PrimMonad
    )

runResM :: ResM a -> IO a
runResM m = do
  re <- newResEnv
  a <- R.runResourceT (runReaderT (unResM m) re)
  tids <- readTVarIO (reThreadIdsVar re)
  if Set.null tids
    then do
      me <- atomically (tryReadTMVar (reErrVar re))
      case me of
        Just e -> throwIO e
        Nothing -> pure a
    else error ("Outstanding threads: " ++ show tids)

allocate :: IO a -> (a -> IO ()) -> ResM a
allocate mk free = ResM (fmap snd (R.allocate mk free))

allocate_ :: IO a -> IO () -> ResM ()
allocate_ mk free = ResM (void (R.allocate_ mk free))

register :: IO () -> ResM ()
register free = ResM (void (R.register free))

scoped :: (STM () -> ResM a) -> ResM a
scoped f = ResM $ do
  tidsVar <- asks reThreadIdsVar
  let waitThreads = readTVar tidsVar >>= \tids -> unless (Set.null tids) retry
  liftIO (runResM (f waitThreads))

reraisable :: SomeException -> Bool
reraisable err =
  isNothing (fromException @AsyncCancelled err)
    && isNothing (fromException @AsyncException err)
    && isNothing (fromException @SomeAsyncException err)

spawnWith :: (IO a -> IO (ThreadId, b)) -> IO a -> ResM b
spawnWith k act = ResM $ mask $ \restore -> do
  ResEnv tidsVar errVar <- ask
  startVar <- liftIO newEmptyMVar
  endVar <- liftIO newEmptyTMVarIO
  let mk = k $ runResM $ ResM $ do
        liftIO (readMVar startVar)
        res <- try (restore (liftIO act))
        liftIO $ atomically $ do
          case res of
            Right _ -> pure ()
            Left err -> when (reraisable err) (void (tryPutTMVar errVar (SpawnError err)))
          putTMVar endVar ()
        either (liftIO . throwIO) pure res
      free (cid, _) = do
        atomically $ do
          takeTMVar endVar
          modifyTVar' tidsVar (Set.delete cid)
  b <- fmap (snd . snd) (R.allocate mk free)
  liftIO (putMVar startVar ())
  pure b

spawnThread :: IO () -> ResM ThreadId
spawnThread = spawnWith (\act -> fmap (\tid -> (tid, tid)) (forkIO (catch @AsyncCancelled act (const (pure ())))))

spawnAsync :: IO a -> ResM (Async a)
spawnAsync = spawnWith (fmap (\asy -> (asyncThreadId asy, asy)) . async)

stopThreads :: ResM ()
stopThreads = ResM (asks reThreadIdsVar) >>= \tidsVar -> liftIO (readTVarIO tidsVar >>= go tidsVar Set.empty)
 where
  go tidsVar waitTids killTids = do
    if Set.null waitTids && Set.null killTids
      then pure ()
      else do
        for_ killTids stopThread
        let lastTids = Set.union waitTids killTids
        aliveTids <- atomically $ do
          aliveTids <- readTVar tidsVar
          if Set.null (Set.difference lastTids aliveTids)
            then retry
            else pure aliveTids
        let stillWaitTids = Set.intersection lastTids aliveTids
            nextKillTids = Set.difference aliveTids lastTids
        go tidsVar stillWaitTids nextKillTids

stopThread :: ThreadId -> IO ()
stopThread = flip throwTo AsyncCancelled
