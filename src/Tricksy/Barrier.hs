module Tricksy.Barrier () where

--   ( Barrier
--   , newBarrier
--   , arriveBarrier
--   , trackBarrier
--   )
-- where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, stateTVar)
import Control.Exception (finally)
import Control.Monad (when)
import Tricksy.Active (ActiveVar, deactivateVar)

data Barrier = Barrier
  { barrierCount :: !(TVar Int)
  , barrierActiveVar :: !ActiveVar
  }

newBarrier :: Int -> ActiveVar -> STM Barrier
newBarrier n a = do
  let i = max 0 n
  c <- newTVar i
  when (i <= 0) (deactivateVar a)
  pure (Barrier c a)

arriveBarrier :: Barrier -> STM ()
arriveBarrier (Barrier c a) = do
  i <- stateTVar c (\i -> let j = max 0 (i - 1) in (j, j))
  when (i <= 0) (deactivateVar a)

trackBarrier :: Barrier -> IO a -> IO a
trackBarrier b act = finally act (atomically (arriveBarrier b))
