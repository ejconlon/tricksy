module Tricksy.Active
  ( Active (..)
  , ActiveVar
  , newActiveVar
  , newActiveVarIO
  , readActiveVar
  , readActiveVarIO
  , waitActiveVar
  , deactivateVar
  , trackActive
  )
where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Monad ((>=>))

-- | Signal from consumers to producers
data Active = ActiveNo | ActiveYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Active where
  ActiveNo <> _ = ActiveNo
  ActiveYes <> a = a

instance Monoid Active where
  mempty = ActiveYes
  mappend = (<>)

newtype ActiveVar = ActiveVar {unActiveVar :: TVar Active}

newActiveVar :: STM ActiveVar
newActiveVar = fmap ActiveVar (newTVar ActiveYes)

newActiveVarIO :: IO ActiveVar
newActiveVarIO = fmap ActiveVar (newTVarIO ActiveYes)

readActiveVar :: ActiveVar -> STM Active
readActiveVar = readTVar . unActiveVar

readActiveVarIO :: ActiveVar -> IO Active
readActiveVarIO = readTVarIO . unActiveVar

waitActiveVar :: ActiveVar -> STM ()
waitActiveVar = readActiveVar >=> \case ActiveYes -> retry; ActiveNo -> pure ()

deactivateVar :: ActiveVar -> STM ()
deactivateVar = flip writeTVar ActiveNo . unActiveVar

trackActive :: ActiveVar -> IO a -> IO a
trackActive activeVar act = finally act (atomically (deactivateVar activeVar))
