module Tricksy.Scope
  ( Scope
  , scoped
  , spawn
  )
where

import Control.Concurrent (ThreadId, killThread, myThreadId)
import Control.Concurrent.Async (Async, AsyncCancelled, async, asyncThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, newTMVarIO, putTMVar, takeTMVar, tryPutTMVar, tryReadTMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, swapTVar)
import Control.Exception (AsyncException, Exception (..), SomeException, catch, finally, mask, throwIO)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set

-- Roughly following https://hackage.haskell.org/package/ki-1.0.1.0/docs/src/Ki.Internal.Scope.html#Scope

data ScopeStatus = ScopeStatusOpen | ScopeStatusClosed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data SpawnError = SpawnError
  deriving stock (Eq, Ord, Show)

instance Exception SpawnError

data Scope s = Scope
  { scopeParentId :: !ThreadId
  , scopeStatusVar :: !(TMVar ScopeStatus)
  , scopeChildExcVar :: !(TMVar SomeException)
  , scopeChildIdsVar :: !(TVar (Set ThreadId))
  }

newScope :: ThreadId -> IO (Scope s)
newScope pid = Scope pid <$> newTMVarIO ScopeStatusOpen <*> newEmptyTMVarIO <*> newTVarIO Set.empty

scoped :: (forall s. Scope s -> IO a) -> IO a
scoped act = do
  pid <- myThreadId
  scope <- newScope pid
  finally (act scope) $ do
    (childIds, mayExc) <- atomically $ do
      void (takeTMVar (scopeStatusVar scope))
      childIds <- swapTVar (scopeChildIdsVar scope) Set.empty
      mayExc <- tryReadTMVar (scopeChildExcVar scope)
      pure (childIds, mayExc)
    for_ childIds killThread
    atomically (putTMVar (scopeStatusVar scope) ScopeStatusClosed)
    maybe (pure ()) throwIO mayExc

isUsefulException :: SomeException -> Bool
isUsefulException e = isNothing (fromException @AsyncCancelled e) && isNothing (fromException @AsyncException e)

execute :: Scope s -> TMVar () -> IO a -> IO a
execute scope lockVar act =
  catch (atomically (takeTMVar lockVar) *> act) $ \e -> do
    cid <- myThreadId
    atomically $ do
      when (isUsefulException e) (void (tryPutTMVar (scopeChildExcVar scope) e))
      modifyTVar (scopeChildIdsVar scope) (Set.delete cid)
    throwIO e

spawn :: Scope s -> IO a -> IO (Async a)
spawn scope act = mask $ \restore -> do
  status <- atomically (takeTMVar (scopeStatusVar scope))
  flip finally (atomically (putTMVar (scopeStatusVar scope) status)) $ do
    case status of
      ScopeStatusOpen -> do
        lockVar <- newEmptyTMVarIO
        asy <- async (restore (execute scope lockVar act))
        atomically $ do
          modifyTVar (scopeChildIdsVar scope) (Set.insert (asyncThreadId asy))
          putTMVar lockVar ()
        pure asy
      ScopeStatusClosed -> throwIO SpawnError
