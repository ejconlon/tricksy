{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Tricksy.Monad
  ( ReraiseError (..)
  , ResM
  , runResM
  , allocate
  , allocate_
  , register
  , scoped
  , spawnThread
  , spawnAsync
  , stopThread
  )
where

import Control.Concurrent (ThreadId, forkIO, myThreadId, throwTo)
import Control.Concurrent.Async (Async (..), AsyncCancelled (..), async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Exception (AsyncException, SomeAsyncException, SomeException, catch, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (Exception (fromException), MonadCatch, MonadMask (..), MonadThrow, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.Trans.Resource (ResIO)
import Control.Monad.Trans.Resource qualified as R
import Data.Maybe (isNothing)

newtype ReraiseError = ReraiseError {unReraiseError :: SomeException}
  deriving stock (Show)

instance Exception ReraiseError

newtype ResEnv = ResEnv {reTcVar :: TVar Int}

newResEnv :: IO ResEnv
newResEnv = fmap ResEnv (newTVarIO 0)

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
runResM m = newResEnv >>= R.runResourceT . runReaderT (unResM m)

allocate :: IO a -> (a -> IO ()) -> ResM a
allocate mk free = ResM (fmap snd (R.allocate mk free))

allocate_ :: IO a -> IO () -> ResM ()
allocate_ mk free = ResM (void (R.allocate_ mk free))

register :: IO () -> ResM ()
register free = ResM (void (R.register free))

scoped :: (STM () -> ResM a) -> ResM a
scoped f = ResM $ do
  tcVar <- asks reTcVar
  let waitThreads = readTVar tcVar >>= \i -> unless (i == 0) retry
  liftIO (runResM (f waitThreads))

reraisable :: SomeException -> Bool
reraisable err =
  isNothing (fromException @AsyncCancelled err)
    && isNothing (fromException @AsyncException err)
    && isNothing (fromException @SomeAsyncException err)

spawnWith :: (IO a -> IO (ThreadId, b)) -> IO a -> ResM b
spawnWith k act = ResM $ mask $ \restore -> do
  pid <- liftIO myThreadId
  startVar <- liftIO newEmptyMVar
  endVar <- liftIO newEmptyMVar
  tcVar <- asks reTcVar
  liftIO (atomically (modifyTVar' tcVar succ))
  (cid, b) <- liftIO $ k $ runResM $ ResM $ do
    liftIO (readMVar startVar)
    res <- try (restore (liftIO act))
    case res of
      Right _ -> pure ()
      Left err -> when (reraisable err) (liftIO (throwTo pid (ReraiseError err)))
    liftIO (putMVar endVar ())
    either (liftIO . throwIO) pure res
  void (R.register (stopThread cid *> readMVar endVar *> atomically (modifyTVar' tcVar pred)))
  liftIO (putMVar startVar ())
  pure b

spawnThread :: IO () -> ResM ThreadId
spawnThread = spawnWith (\act -> fmap (\tid -> (tid, tid)) (forkIO (catch @AsyncCancelled act (const (pure ())))))

spawnAsync :: IO a -> ResM (Async a)
spawnAsync = spawnWith (fmap (\asy -> (asyncThreadId asy, asy)) . async)

stopThread :: ThreadId -> IO ()
stopThread = flip throwTo AsyncCancelled
