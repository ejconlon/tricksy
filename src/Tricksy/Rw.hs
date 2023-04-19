module Tricksy.Rw
  ( Rw (..)
  , varRw
  , chanRw
  , lockRw
  , ringRw
  , ringRwIO
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Tricksy.Ring (Ring, cursorAdvance, newCursor, newCursorIO, ringWrite)

-- | Interface abstracting over reading and writing to containers in STM.
data Rw a b = Rw
  { rwWrite :: !(a -> STM ())
  , rwRead :: !(STM b)
  }
  deriving (Functor)

varRw :: TVar a -> Rw a a
varRw var = Rw (writeTVar var) (readTVar var)

chanRw :: TChan a -> Rw a a
chanRw chan = Rw (writeTChan chan) (readTChan chan)

lockRw :: TMVar a -> Rw a a
lockRw lock = Rw (putTMVar lock) (takeTMVar lock)

ringRw :: Ring a -> STM (Rw a (Int, a))
ringRw ring = do
  cur <- newCursor ring
  let wr = ringWrite ring
      rd = cursorAdvance cur
  pure (Rw wr rd)

ringRwIO :: Ring a -> IO (Rw a (Int, a))
ringRwIO ring = do
  cur <- newCursorIO ring
  let wr = ringWrite ring
      rd = cursorAdvance cur
  pure (Rw wr rd)
