module Tricksy.Cache
  ( CacheHandler
  , defaultCacheHandler
  , Cache
  , newCache
  , readCache
  , finalizeCache
  , testCache
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVar, readTMVar, throwSTM)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, tryPutTMVar)
import Control.Exception (SomeException, mask, try)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import GHC.Conc (unsafeIOToSTM)
import Tricksy.Active (Active (..), ActiveVar, deactivateVar, newActiveVarIO, readActiveVar)
import Tricksy.Scope (Scope, scoped, spawnThread)
import Tricksy.Time (MonoTime, TimeDelta, currentMonoTime, diffMonoTime, threadDelayDelta, timeDeltaFromFracSecs)

type CacheHandler z a = Maybe (Either SomeException z) -> STM a

defaultCacheHandler :: a -> CacheHandler a a
defaultCacheHandler a = maybe (pure a) (either throwSTM pure)

data CacheStatus = CacheStatusOpen | CacheStatusClosing | CacheStatusClosed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data CacheState z = CacheState
  { csStatus :: !CacheStatus
  , csTime :: !(Maybe MonoTime)
  , csActiveVar :: !ActiveVar
  , csResVar :: !(TMVar (Either SomeException z))
  }

data Cache s a where
  Cache :: !(Scope s) -> !TimeDelta -> !(CacheHandler z a) -> !(IO z) -> !(MVar (CacheState z)) -> Cache s a

newCache :: Scope s -> TimeDelta -> CacheHandler z a -> IO z -> IO (Cache s a)
newCache scope ttl han act = do
  activeVar <- newActiveVarIO
  resVar <- newEmptyTMVarIO
  stVar <- newMVar (CacheState CacheStatusOpen Nothing activeVar resVar)
  pure (Cache scope ttl han act stVar)

readCache :: Cache s a -> STM a
readCache (Cache scope ttl han act stVar) = go
 where
  go = do
    newResVar <- newEmptyTMVar
    useResVar <- unsafeIOToSTM (request newResVar)
    res <- readTMVar useResVar
    han (Just res)
  request newResVar = mask $ \restore ->
    modifyMVar stVar $ \st@(CacheState stat mprev activeVar oldResVar) -> do
      case stat of
        CacheStatusOpen -> do
          now <- currentMonoTime
          let withinTtl = fromMaybe False $ do
                prev <- mprev
                td <- diffMonoTime now prev
                pure (td < ttl)
          if withinTtl
            then pure (st, oldResVar)
            else do
              void (spawnThread scope (try (restore act) >>= atomically . putIfActive activeVar newResVar))
              pure (st {csTime = Just now, csResVar = newResVar}, newResVar)
        _ -> pure (st, oldResVar)

putIfActive :: ActiveVar -> TMVar (Either SomeException z) -> Either SomeException z -> STM ()
putIfActive activeVar resVar res = do
  active <- readActiveVar activeVar
  case active of
    ActiveNo -> pure ()
    ActiveYes -> void (tryPutTMVar resVar res)

finalizeCache :: Cache s a -> IO ()
finalizeCache (Cache _ _ _ _ stVar) = mask $ \restore -> do
  mayVars <- modifyMVar stVar $ \st@(CacheState stat _ activeVar resVar) -> do
    case stat of
      CacheStatusClosed -> pure (st, Nothing)
      _ -> pure (st {csStatus = CacheStatusClosing}, Just (activeVar, resVar))
  case mayVars of
    Nothing -> pure ()
    Just (activeVar, resVar) -> do
      restore $ atomically $ do
        deactivateVar activeVar
        void (readTMVar resVar)
      modifyMVar_ stVar (\st -> pure (st {csStatus = CacheStatusClosed}))

testCache :: IO ()
testCache = do
  putStrLn "starting"
  let han = defaultCacheHandler 0
  ref <- newIORef (1 :: Int)
  let act = putStrLn "fetching" *> readIORef ref
      ttl = timeDeltaFromFracSecs (0.5 :: Double)
      assertEq a b = if a == b then pure () else fail ("Mismatch: " ++ show a ++ " vs " ++ show b)
  -- Test with requests
  scoped $ \scope -> do
    cache <- newCache scope ttl han act
    v1 <- atomically (readCache cache)
    assertEq v1 1
    writeIORef ref 2
    v2 <- atomically (readCache cache)
    assertEq v2 1
    threadDelayDelta ttl
    threadDelayDelta ttl
    v3 <- atomically (readCache cache)
    assertEq v3 2
    writeIORef ref 3
    finalizeCache cache
    threadDelayDelta ttl
    threadDelayDelta ttl
    v4 <- atomically (readCache cache)
    assertEq v4 2
  -- Test without requests
  scoped $ \scope -> do
    cache <- newCache scope ttl han act
    finalizeCache cache
    v1 <- atomically (readCache cache)
    assertEq v1 0
  putStrLn "done"
