module Tricksy.Rw
  ( Rw (..)
  , chanRw
  , newChanRw
  , lockRw
  , newLockRw
  , ringRw
  , ringRwIO
  , newRingRw
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Monad ((>=>))
import Tricksy.Ring (Next, Ring, cursorNext, newCursor, newCursorIO, newRingIO, ringWrite)

-- | Interface abstracting over reading and writing to containers in STM.
data Rw a b = Rw
  { rwWrite :: !(a -> STM ())
  , rwRead :: !(STM b)
  }
  deriving (Functor)

chanRw :: TChan a -> Rw a a
chanRw chan = Rw (writeTChan chan) (readTChan chan)

newChanRw :: IO (Rw a a)
newChanRw = fmap chanRw newTChanIO

lockRw :: TMVar a -> Rw a a
lockRw lock = Rw (putTMVar lock) (takeTMVar lock)

newLockRw :: IO (Rw a a)
newLockRw = fmap lockRw newEmptyTMVarIO

ringRw :: Ring a -> STM (Rw a (Next a))
ringRw ring = do
  cur <- newCursor ring
  let wr = ringWrite ring
      rd = cursorNext cur
  pure (Rw wr rd)

ringRwIO :: Ring a -> IO (Rw a (Next a))
ringRwIO ring = do
  cur <- newCursorIO ring
  let wr = ringWrite ring
      rd = cursorNext cur
  pure (Rw wr rd)

newRingRw :: Int -> IO (Rw a (Next a))
newRingRw = newRingIO >=> ringRwIO
