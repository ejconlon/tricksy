module Tricksy.Scope
  ( Scope
  , scoped
  , spawnThread
  , spawnAsync
  , stopThread
  , stopAsync
  )
where

import Control.Concurrent (ThreadId, forkIO, myThreadId, takeMVar)
import Control.Concurrent.Async (Async, AsyncCancelled (..), async, asyncThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar)
import Control.Exception (Exception (..), finally, mask, throwIO, throwTo)
import Data.Foldable (for_)
import Data.Set (Set)
import Data.Set qualified as Set

data SpawnError = SpawnError
  deriving stock (Eq, Ord, Show)

instance Exception SpawnError

data ScopeStatus = ScopeStatusOpen | ScopeStatusClosed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data ScopeState = ScopeState
  { ssStatus :: !ScopeStatus
  , ssChildIds :: !(Set ThreadId)
  }
  deriving stock (Eq, Ord, Show)

data Scope s = Scope
  { scopeParentId :: !ThreadId
  , scopeStateVar :: !(MVar ScopeState)
  }

newScope :: ThreadId -> IO (Scope s)
newScope pid = Scope pid <$> newMVar (ScopeState ScopeStatusOpen Set.empty)

scoped :: (forall s. Scope s -> IO a) -> IO a
scoped act = do
  pid <- myThreadId
  scope <- newScope pid
  finally (act scope) $ do
    childIds <- modifyMVar (scopeStateVar scope) $ \ss ->
      pure (ScopeState ScopeStatusClosed Set.empty, ssChildIds ss)
    for_ childIds stopThread

execute :: Scope s -> MVar () -> IO a -> IO a
execute scope lockVar act = do
  cid <- myThreadId
  finally (takeMVar lockVar *> act) $
    modifyMVar_ (scopeStateVar scope) $ \ss ->
      pure ss {ssChildIds = Set.delete cid (ssChildIds ss)}

spawnWith :: (IO a -> IO (ThreadId, b)) -> Scope s -> IO a -> IO b
spawnWith k scope act = mask $ \restore -> do
  (ref, lockVar) <- modifyMVar (scopeStateVar scope) $ \ss ->
    case ssStatus ss of
      ScopeStatusOpen -> do
        lockVar <- newEmptyMVar
        (cid, ref) <- k (restore (execute scope lockVar act))
        let ss' = ss {ssChildIds = Set.insert cid (ssChildIds ss)}
        pure (ss', (ref, lockVar))
      ScopeStatusClosed -> throwIO SpawnError
  putMVar lockVar ()
  pure ref

spawnThread :: Scope s -> IO () -> IO ThreadId
spawnThread = spawnWith (fmap (\tid -> (tid, tid)) . forkIO)

spawnAsync :: Scope s -> IO a -> IO (Async a)
spawnAsync = spawnWith (fmap (\asy -> (asyncThreadId asy, asy)) . async)

stopThread :: ThreadId -> IO ()
stopThread = flip throwTo AsyncCancelled

stopAsync :: Async a -> IO ()
stopAsync = stopThread . asyncThreadId
