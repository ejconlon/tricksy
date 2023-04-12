module Tricksy.Active
  ( Active (..)
  , ActiveVar
  , newActiveVarIO
  , readActiveVar
  , readActiveVarIO
  , deactivateVar
  , deactivateVarIO
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)

-- | Signal from consumers to producers
data Active = ActiveYes | ActiveNo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype ActiveVar = ActiveVar {unActiveVar :: TVar Active}

newActiveVarIO :: IO ActiveVar
newActiveVarIO = fmap ActiveVar (newTVarIO ActiveYes)

readActiveVar :: ActiveVar -> STM Active
readActiveVar = readTVar . unActiveVar

readActiveVarIO :: ActiveVar -> IO Active
readActiveVarIO = readTVarIO . unActiveVar

deactivateVar :: ActiveVar -> STM ()
deactivateVar = flip writeTVar ActiveNo . unActiveVar

deactivateVarIO :: ActiveVar -> IO ()
deactivateVarIO = atomically . deactivateVar
