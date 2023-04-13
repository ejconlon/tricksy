module Tricksy.ActiveScope
  ( scopedActive
  )
where

import Control.Exception (finally)
import Tricksy.Active (ActiveVar, deactivateVarIO, newActiveVarIO)
import Tricksy.Scope (Scope, scoped)

scopedActive :: (forall s. ActiveVar -> Scope s -> IO a) -> IO a
scopedActive f = do
  activeVar <- newActiveVarIO
  scoped (\scope -> finally (f activeVar scope) (deactivateVarIO activeVar))
