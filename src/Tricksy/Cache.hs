module Tricksy.Cache
  ( CacheHandler
  , defaultCacheHandler
  , runCache
  )
where

import Control.Concurrent.STM (STM, atomically, retry, throwSTM)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Conc (unsafeIOToSTM)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Tricksy.Active (Active (..))
import Tricksy.Control (Control (..), Ref, guardedMay, guarded_, mkRef, newControl, viewRef)
import Tricksy.Monad (ResM, finallyRegister, runResM, scoped, spawnThread)
import Tricksy.Time (MonoTime, TimeDelta, currentMonoTime, diffMonoTime, threadDelayDelta, timeDeltaFromFracSecs)

type CacheHandler z a = Maybe (Either SomeException z) -> STM a

defaultCacheHandler :: a -> CacheHandler a a
defaultCacheHandler a = maybe (pure a) (either throwSTM pure)

data ReqState a = ReqStateInit | ReqStateProcessing | ReqStateDone !a
  deriving stock (Eq, Ord, Show)

data Request a = Request
  { reqTime :: !MonoTime
  , reqStateVar :: !(TVar (ReqState a))
  }

data CacheEnv z a = CacheEnv
  { ceControl :: !Control
  , ceTtl :: !TimeDelta
  , ceHandler :: !(CacheHandler z a)
  , ceAction :: !(IO z)
  , ceReqVar :: !(TVar (Maybe (Request a)))
  , ceFinalVar :: !(TVar (Maybe a))
  }

-- Dangerous?
monoTimeSTM :: STM MonoTime
monoTimeSTM = unsafeIOToSTM currentMonoTime

-- Read the last known value
lastVal :: CacheEnv z a -> STM (Maybe a)
lastVal ce = do
  mreq <- readTVar (ceReqVar ce)
  case mreq of
    Nothing -> pure Nothing
    Just req -> do
      flip fmap (readTVar (reqStateVar req)) $ \case
        ReqStateDone a -> Just a
        _ -> Nothing

-- Ensure a value on termination
ensureFinalVar :: CacheEnv z a -> STM a
ensureFinalVar ce = do
  ma <- readTVar (ceFinalVar ce)
  case ma of
    Nothing -> do
      mval <- lastVal ce
      val <- maybe (ceHandler ce Nothing) pure mval
      writeTVar (ceFinalVar ce) (Just val)
      pure val
    Just val -> pure val

cacheRead :: CacheEnv z a -> TVar (ReqState a) -> STM a
cacheRead ce stVar = do
  active <- controlReadActive (ceControl ce)
  case active of
    ActiveNo -> ensureFinalVar ce
    ActiveYes -> do
      -- Use the newest available value
      mval <- lastVal ce
      case mval of
        Just val -> pure val
        Nothing -> do
          -- Fallback to this value, if present
          st <- readTVar stVar
          case st of
            ReqStateDone a -> pure a
            _ -> retry

mkNewReq :: CacheEnv z a -> MonoTime -> STM (TVar (ReqState a))
mkNewReq ce now = do
  newReq <- Request now <$> newTVar ReqStateInit
  writeTVar (ceReqVar ce) (Just newReq)
  pure (reqStateVar newReq)

cacheRequest :: CacheEnv z a -> Ref a
cacheRequest ce = mkRef $ do
  active <- controlReadActive (ceControl ce)
  case active of
    ActiveNo -> pure (ensureFinalVar ce)
    ActiveYes -> do
      mayOldReq <- readTVar (ceReqVar ce)
      now <- monoTimeSTM
      resVar <- case mayOldReq of
        Nothing -> mkNewReq ce now
        Just oldReq -> do
          let withinTtl = maybe False (< ceTtl ce) (diffMonoTime now (reqTime oldReq))
          if withinTtl
            then pure (reqStateVar oldReq)
            else mkNewReq ce now
      pure (cacheRead ce resVar)

cacheFork :: CacheEnv z a -> IO ()
cacheFork ce = runResM go
 where
  go = do
    mreq <- liftIO $ atomically $ do
      guardedMay (controlReadActive (ceControl ce)) $ do
        mreq <- readTVar (ceReqVar ce)
        case mreq of
          Nothing -> retry
          Just req -> do
            st <- readTVar (reqStateVar req)
            case st of
              ReqStateInit -> do
                writeTVar (reqStateVar req) ReqStateProcessing
                pure mreq
              _ -> retry
    case mreq of
      Nothing -> pure ()
      Just req ->
        void (spawnThread (cacheFetch (ceControl ce) (ceHandler ce) (ceAction ce) req)) *> go

cacheFetch :: Control -> CacheHandler z a -> IO z -> Request a -> IO ()
cacheFetch ctl han act req = guarded_ (atomically (controlReadActive ctl)) $ do
  res <- try act
  atomically $ guarded_ (controlReadActive ctl) $ do
    st <- readTVar (reqStateVar req)
    case st of
      ReqStateProcessing -> do
        val <- han (Just res)
        writeTVar (reqStateVar req) (ReqStateDone val)
      _ -> pure ()

cacheDispose :: CacheEnv z a -> STM ()
cacheDispose ce = controlDeactivate (ceControl ce)

runCache :: TimeDelta -> CacheHandler z a -> IO z -> ResM (Ref a, STM ())
runCache ttl han act = do
  ctl <- liftIO newControl
  ce <- liftIO (CacheEnv ctl ttl han act <$> newTVarIO Nothing <*> newTVarIO Nothing)
  let dispose = cacheDispose ce
  finallyRegister (void (spawnThread (cacheFork ce))) (atomically dispose)
  pure (cacheRequest ce, dispose)

testCache :: IO ()
testCache = do
  hSetBuffering stdout LineBuffering
  putStrLn "starting"
  let han = defaultCacheHandler 0
  ref <- newIORef (1 :: Int)
  let act = putStrLn "fetching" *> readIORef ref
      ttl = timeDeltaFromFracSecs (0.5 :: Double)
      assertEq v a b = if a == b then pure () else fail ("Mismatch on " ++ v ++ ": actual " ++ show a ++ " vs expected " ++ show b)
  -- Test with requests
  scoped $ \_ -> do
    (readCache, dispose) <- runCache ttl han act
    liftIO $ do
      v1 <- viewRef readCache
      assertEq "v1" v1 1
      writeIORef ref 2
      v2 <- viewRef readCache
      assertEq "v2" v2 1
      threadDelayDelta ttl
      threadDelayDelta ttl
      v3 <- viewRef readCache
      assertEq "v3" v3 2
      writeIORef ref 3
      atomically dispose
      threadDelayDelta ttl
      threadDelayDelta ttl
      v4 <- viewRef readCache
      assertEq "v4" v4 2
  -- Test without requests
  scoped $ \_ -> do
    (readCache, dispose) <- runCache ttl han act
    liftIO $ do
      atomically dispose
      v1 <- viewRef readCache
      assertEq "v1" v1 0
  putStrLn "done"
