module Tricksy.Rw
  ( Rw (..)
  , chanRw
  , lockRw
  , ringRw
  )
where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, takeTMVar)
import Tricksy.Ring (cursorAdvanceRead, newCursor, newRingIO, ringWrite)

-- | Interface abstracting over reading and writing to containers in STM.
data Rw a b = Rw
  { rwWrite :: !(a -> STM ())
  , rwRead :: !(STM b)
  }
  deriving (Functor)

chanRw :: IO (Rw a a)
chanRw = do
  chan <- newTChanIO
  pure (Rw (writeTChan chan) (readTChan chan))

lockRw :: IO (Rw a a)
lockRw = do
  lock <- newEmptyTMVarIO
  pure (Rw (putTMVar lock) (takeTMVar lock))

ringRw :: Int -> a -> IO (Rw a (Int, a))
ringRw cap initVal = do
  ring <- newRingIO cap initVal
  cur <- atomically (fmap fst (newCursor ring))
  let wr = ringWrite ring
      rd = do
        p@(i, _) <- cursorAdvanceRead cur
        if i == 0 then retry else pure p
  pure (Rw wr rd)
