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
  )
where

import Control.Concurrent (ThreadId, forkIO, myThreadId, throwTo)
import Control.Concurrent.Async (Async (..), AsyncCancelled (..), async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (AsyncException, SomeAsyncException, SomeException, throwIO)
import Control.Monad (void, when)
import Control.Monad.Catch (Exception (fromException), MonadCatch, MonadMask (..), MonadThrow, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Trans.Resource (ResIO)
import Control.Monad.Trans.Resource qualified as R
import Data.Maybe (isNothing)

newtype ReraiseError = ReraiseError {unReraiseError :: SomeException}
  deriving stock (Show)

instance Exception ReraiseError

newtype ResM a = ResM {unResM :: ResIO a}
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
runResM = R.runResourceT . unResM

allocate :: IO a -> (a -> IO ()) -> ResM a
allocate mk free = ResM (fmap snd (R.allocate mk free))

allocate_ :: IO a -> IO () -> ResM ()
allocate_ mk free = ResM (void (R.allocate_ mk free))

register :: IO () -> ResM ()
register free = ResM (void (R.register free))

scoped :: ResM a -> ResM a
scoped = liftIO . runResM

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
  (cid, b) <- liftIO $ k $ R.runResourceT $ do
    liftIO (readMVar startVar)
    res <- try (restore (liftIO act))
    liftIO (putMVar endVar ())
    case res of
      Right _ -> pure ()
      Left err -> when (reraisable err) (liftIO (throwTo pid (ReraiseError err)))
    either (liftIO . throwIO) pure res
  void (R.register (throwTo cid AsyncCancelled *> readMVar endVar))
  liftIO (putMVar startVar ())
  pure b

spawnThread :: IO () -> ResM ThreadId
spawnThread = spawnWith (fmap (\tid -> (tid, tid)) . forkIO)

spawnAsync :: IO a -> ResM (Async a)
spawnAsync = spawnWith (fmap (\asy -> (asyncThreadId asy, asy)) . async)
