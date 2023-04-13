module Tricksy.Active
  ( Active (..)
  , ActiveVar
  , newActiveVarIO
  , readActiveVar
  , readActiveVarIO
  , awaitActiveVar
  , awaitActiveVarIO
  , deactivateVar
  , deactivateVarIO
  )
where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad ((>=>))

-- | Signal from consumers to producers
data Active = ActiveNo | ActiveYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- instance Semigroup Active where
--   a <> b =
--     case a of
--       ActiveNo -> ActiveNo
--       ActiveYes -> b

-- instance Monoid Active where
--   mempty = ActiveYes
--   mappend = (<>)

newtype ActiveVar = ActiveVar {unActiveVar :: TVar Active}

newActiveVarIO :: IO ActiveVar
newActiveVarIO = fmap ActiveVar (newTVarIO ActiveYes)

readActiveVar :: ActiveVar -> STM Active
readActiveVar = readTVar . unActiveVar

readActiveVarIO :: ActiveVar -> IO Active
readActiveVarIO = readTVarIO . unActiveVar

awaitActiveVar :: ActiveVar -> STM ()
awaitActiveVar = readActiveVar >=> \case ActiveYes -> retry; ActiveNo -> pure ()

awaitActiveVarIO :: ActiveVar -> IO ()
awaitActiveVarIO = atomically . awaitActiveVar

deactivateVar :: ActiveVar -> STM ()
deactivateVar = flip writeTVar ActiveNo . unActiveVar

deactivateVarIO :: ActiveVar -> IO ()
deactivateVarIO = atomically . deactivateVar
