module Tricksy.ActiveScope
  ( allocateActiveVar
  , scopedActive
  )
where

import Control.Concurrent.STM (atomically, orElse)
import Control.Monad.IO.Class (liftIO)
import Tricksy.Active (ActiveVar, deactivateVar, newActiveVarIO, waitActiveVar)
import Tricksy.Monad (ResM, allocate, scoped)

allocateActiveVar :: ResM ActiveVar
allocateActiveVar = allocate newActiveVarIO (atomically . deactivateVar)

scopedActive :: (ActiveVar -> ResM a) -> ResM a
scopedActive f = scoped $ \waitThreads -> do
  activeVar <- allocateActiveVar
  a <- f activeVar
  liftIO (atomically (waitActiveVar activeVar `orElse` waitThreads))
  pure a
