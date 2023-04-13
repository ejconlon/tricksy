module Tricksy.ActiveScope
  ( scopedActive
  , spawnActive
  ) where

import Tricksy.Active (newActiveVarIO, deactivateVarIO, ActiveVar)
import Tricksy.Scope (Scope, scoped, spawn)
import Control.Exception (finally, onException)
import Control.Concurrent.Async (Async)

scopedActive :: (forall s. ActiveVar -> Scope s -> IO a) -> IO a
scopedActive f = do
  activeVar <- newActiveVarIO
  scoped (\scope -> finally (f activeVar scope) (deactivateVarIO activeVar))

spawnActive :: ActiveVar -> Scope s -> IO a -> IO (Async a)
spawnActive activeVar scope act = spawn scope (onException act (deactivateVarIO activeVar))
