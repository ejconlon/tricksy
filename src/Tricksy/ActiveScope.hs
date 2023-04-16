module Tricksy.ActiveScope
  ( allocateActiveVar
  , scopedActive
  )
where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import Tricksy.Active (ActiveVar, awaitActiveVar, deactivateVar, newActiveVarIO)
import Tricksy.Monad (ResM, allocate, scoped)

allocateActiveVar :: ResM ActiveVar
allocateActiveVar = allocate newActiveVarIO (atomically . deactivateVar)

scopedActive :: (ActiveVar -> ResM a) -> ResM a
scopedActive f = scoped $ do
  activeVar <- allocateActiveVar
  a <- f activeVar
  liftIO (atomically (awaitActiveVar activeVar))
  pure a
