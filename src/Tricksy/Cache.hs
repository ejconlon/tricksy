module Tricksy.Cache
  ( Cache
  , newCache
  , readCache
  , testCache
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVar, readTMVar, throwSTM)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, tryTakeTMVar)
import Control.Exception (SomeException, mask, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Conc (unsafeIOToSTM)
import Tricksy.Scope (Scope, scoped, spawnThread)
import Tricksy.Time (MonoTime, TimeDelta, currentMonoTime, diffMonoTime, threadDelayDelta, timeDeltaFromFracSecs)

data CacheState a = CacheState
  { csTime :: !MonoTime
  , csThread :: !ThreadId
  , csVar :: !(TMVar (Either SomeException a))
  }

data Cache s a = Cache
  { cacheScope :: !(Scope s)
  , cacheTTL :: !TimeDelta
  , cacheAction :: !(IO a)
  , cacheReqVar :: !(MVar (Maybe (CacheState a)))
  }

newCache :: Scope s -> TimeDelta -> IO a -> IO (Cache s a)
newCache scope ttl act = Cache scope ttl act <$> newMVar Nothing

readCache :: Cache s a -> STM a
readCache (Cache scope ttl act reqVar) = res
 where
  res = do
    v <- newEmptyTMVar
    w <- unsafeIOToSTM (go v)
    ea <- readTMVar w
    either throwSTM pure ea
  go v = mask $ \restore ->
    modifyMVar reqVar $ \mx -> do
      u <- currentMonoTime
      let mw = do
            CacheState t _ w <- mx
            td <- diffMonoTime u t
            if td < ttl then Just w else Nothing
      case mw of
        Just w -> pure (mx, w)
        Nothing -> do
          -- Something fishy here re async exceptions in the thread body...
          i <- spawnThread scope (try (restore act) >>= atomically . writeTMVar v)
          pure (Just (CacheState u i v), v)

-- This is not in the version of stm used here
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar v a = tryTakeTMVar v *> putTMVar v a

testCache :: IO ()
testCache = do
  putStrLn "starting"
  ref <- newIORef (0 :: Int)
  let act = putStrLn "fetching" *> readIORef ref
      ttl = timeDeltaFromFracSecs (0.5 :: Double)
      assertEq a b = if a == b then pure () else fail ("Mismatch: " ++ show a ++ " vs " ++ show b)
  scoped $ \scope -> do
    cache <- newCache scope ttl act
    v1 <- atomically (readCache cache)
    assertEq v1 0
    writeIORef ref 1
    v2 <- atomically (readCache cache)
    assertEq v2 0
    threadDelayDelta ttl
    threadDelayDelta ttl
    v3 <- atomically (readCache cache)
    assertEq v3 1
  putStrLn "done"
