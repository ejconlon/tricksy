module Tricksy.Barrier
  ( Barrier
  , newBarrier
  , arriveBarrier
  , trackBarrier
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, stateTVar)
import Control.Exception (finally)
import Control.Monad (when)

data Barrier = Barrier
  { barrierCount :: !(TVar Int)
  , barrierDeactivate :: !(STM ())
  }

newBarrier :: Int -> STM () -> STM Barrier
newBarrier n d = do
  let i = max 0 n
  c <- newTVar i
  when (i <= 0) d
  pure (Barrier c d)

arriveBarrier :: Barrier -> STM ()
arriveBarrier (Barrier c d) = do
  i <- stateTVar c (\i -> let j = max 0 (i - 1) in (j, j))
  when (i <= 0) d

trackBarrier :: Barrier -> IO a -> IO a
trackBarrier b act = finally act (atomically (arriveBarrier b))
