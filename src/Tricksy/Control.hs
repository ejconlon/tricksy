module Tricksy.Control
  ( Control (..)
  , allocateControl
  , scopedControl
  , trackControl
  )
where

import Control.Concurrent.STM (STM, atomically, orElse)
import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import Tricksy.Active (Active (..), ActiveVar, deactivateVar, newActiveVarIO, readActiveVar, waitActiveVar)
import Tricksy.Monad (ResM, allocate, scoped)

data Control = Control
  { controlReadActive :: !(STM Active)
  , controlDeactivate :: !(STM ())
  , controlWait :: !(STM ())
  }

activeVarControl :: ActiveVar -> Control
activeVarControl v = Control (readActiveVar v) (deactivateVar v) (waitActiveVar v)

allocateActiveVar :: ResM ActiveVar
allocateActiveVar = allocate newActiveVarIO (atomically . deactivateVar)

allocateControl :: ResM Control
allocateControl = fmap activeVarControl allocateActiveVar

scopedControl :: Control -> ResM () -> ResM ()
scopedControl ctl act = do
  active <- liftIO (atomically (controlReadActive ctl))
  case active of
    ActiveNo -> pure ()
    ActiveYes -> scoped $ \waitThreads ->
      act *> liftIO (atomically (orElse (controlWait ctl) waitThreads))

trackControl :: Control -> IO a -> IO a
trackControl ctl act = finally act (atomically (controlDeactivate ctl))
