module Tricksy.Ring
  ( Ring
  , ringCap
  , newRingIO
  , newRing
  , ringWrite
  , Cursor
  , newCursor
  , newCursorIO
  , cursorAdvance
  , cursorFlush
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Data.Sequence (Seq)
import Data.Vector (Vector)
import Data.Vector qualified as V

data Pos = Pos
  { posIndex :: !Int
  , posGen :: !Int
  }
  deriving stock (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 0 0

nextPos :: Int -> Pos -> Pos
nextPos cap (Pos ix gen) =
  let ix' = ix + 1
  in  if ix' == cap
        then Pos 0 (gen + 1)
        else Pos ix' gen

data Ring a = Ring
  { ringCap :: !Int
  , ringBuf :: !(Vector (TVar a))
  , ringHead :: !(TVar Pos)
  }

data Cursor a = Cursor
  { cursorRing :: !(Ring a)
  , cursorHead :: !(TVar Pos)
  }

newRingIO :: Int -> IO (Ring a)
newRingIO cap = Ring cap <$> V.generateM cap (const (newTVarIO undefined)) <*> newTVarIO initPos

newRing :: Int -> STM (Ring a)
newRing cap = Ring cap <$> V.generateM cap (const (newTVar undefined)) <*> newTVar initPos

ringWrite :: Ring a -> a -> STM ()
ringWrite (Ring cap buf hdVar) val = do
  ix <- stateTVar hdVar (\hd -> let hd'@(Pos ix _) = nextPos cap hd in (ix, hd'))
  writeTVar (buf V.! ix) val

newCursor :: Ring a -> STM (Cursor a)
newCursor r = do
  hd <- readTVar (ringHead r)
  v <- newTVar hd
  pure (Cursor r v)

newCursorIO :: Ring a -> IO (Cursor a)
newCursorIO r = do
  hd <- readTVarIO (ringHead r)
  v <- newTVarIO hd
  pure (Cursor r v)

cursorAdvance :: Cursor a -> STM (Int, a)
cursorAdvance = undefined

cursorFlush :: Cursor a -> STM (Seq (Int, a))
cursorFlush = undefined
