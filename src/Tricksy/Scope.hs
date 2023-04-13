module Tricksy.Scope
  ( Scope
  , scoped
  , spawn
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, AsyncCancelled (..), async, asyncThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, newTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, swapTVar)
import Control.Exception (Exception (..), finally, mask, throwIO, throwTo)
import Control.Monad (void)
import Data.Foldable (for_)
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
  , scopeChildIdsVar :: !(TVar (Set ThreadId))
  }

newScope :: ThreadId -> IO (Scope s)
newScope pid = Scope pid <$> newTMVarIO ScopeStatusOpen <*> newTVarIO Set.empty

scoped :: (forall s. Scope s -> IO a) -> IO a
scoped act = do
  pid <- myThreadId
  scope <- newScope pid
  finally (act scope) $ do
    childIds <- atomically $ do
      void (takeTMVar (scopeStatusVar scope))
      swapTVar (scopeChildIdsVar scope) Set.empty
    for_ childIds (`throwTo` AsyncCancelled)
    atomically (putTMVar (scopeStatusVar scope) ScopeStatusClosed)

execute :: Scope s -> TMVar () -> IO a -> IO a
execute scope lockVar act = do
  cid <- myThreadId
  finally (atomically (takeTMVar lockVar) *> act) $
    atomically (modifyTVar (scopeChildIdsVar scope) (Set.delete cid))

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
