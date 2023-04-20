module Tricksy.Control
  ( Control (..)
  , controlReadActiveIO
  , controlDeactivateIO
  , newControl
  , allocateControl
  , scopedControl
  , trackControl
  , guarded
  , guarded_
  , guardedMay
  , Ref (..)
  , mkRef
  , peelRef
  , viewRef
  )
where

import Control.Applicative (Applicative (..))
import Control.Concurrent.STM (STM, atomically, orElse)
import Control.Exception (finally)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Tricksy.Active (Active (..), ActiveVar, deactivateVar, newActiveVarIO, readActiveVar, waitActiveVar)
import Tricksy.Monad (ResM, allocate, scoped)

data Control = Control
  { controlReadActive :: !(STM Active)
  , controlDeactivate :: !(STM ())
  , controlWait :: !(STM ())
  }

controlReadActiveIO :: Control -> IO Active
controlReadActiveIO = atomically . controlReadActive

controlDeactivateIO :: Control -> IO ()
controlDeactivateIO = atomically . controlDeactivate

activeVarControl :: ActiveVar -> Control
activeVarControl v = Control (readActiveVar v) (deactivateVar v) (waitActiveVar v)

allocateActiveVar :: ResM ActiveVar
allocateActiveVar = allocate newActiveVarIO (atomically . deactivateVar)

newControl :: IO Control
newControl = fmap activeVarControl newActiveVarIO

allocateControl :: ResM Control
allocateControl = fmap activeVarControl allocateActiveVar

scopedControl :: Control -> ResM () -> IO ()
scopedControl ctl act = do
  active <- liftIO (atomically (controlReadActive ctl))
  case active of
    ActiveNo -> pure ()
    ActiveYes -> scoped $ \waitThreads ->
      act *> liftIO (atomically (orElse (controlWait ctl) waitThreads))

-- | Deactivate the control when the action has finished
trackControl :: Control -> IO a -> IO a
trackControl ctl act = finally act (atomically (controlDeactivate ctl))

guarded :: Monad m => m Active -> m () -> m Active
guarded rdActive act = do
  active <- rdActive
  case active of
    ActiveNo -> pure ActiveNo
    ActiveYes -> act *> rdActive

guardedDefault :: Monad m => a -> m Active -> m a -> m a
guardedDefault defaultVal rdActive act = do
  active <- rdActive
  case active of
    ActiveNo -> pure defaultVal
    ActiveYes -> act

guarded_ :: Monad m => m Active -> m () -> m ()
guarded_ = guardedDefault ()

guardedMay :: Monad m => m Active -> m (Maybe a) -> m (Maybe a)
guardedMay = guardedDefault Nothing

newtype Ref a = Ref {unRef :: STM (STM a)}

instance Functor Ref where
  fmap f (Ref x) = Ref (fmap (fmap f) x)

instance Applicative Ref where
  pure = Ref . pure . pure
  Ref x <*> Ref y = Ref (liftA2 (<*>) x y)
  liftA2 f (Ref x) (Ref y) = Ref (liftA2 (liftA2 f) x y)
  Ref x *> Ref y = Ref (liftA2 (*>) x y)
  Ref x <* Ref y = Ref (liftA2 (<*) x y)

mkRef :: STM (STM a) -> Ref a
mkRef = Ref

peelRef :: Ref a -> IO (STM a)
peelRef = atomically . unRef

viewRef :: Ref a -> IO a
viewRef = peelRef >=> atomically
