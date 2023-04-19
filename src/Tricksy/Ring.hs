module Tricksy.Ring
  ( Ring
  , ringCap
  , newRingIO
  , newRing
  , ringWrite
  , Cursor
  , newCursor
  -- , newCursorIO
  , cursorRead
  , cursorAdvance
  , cursorAdvanceRead
  , cursorConsume
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

newRingIO :: Int -> a -> IO (Ring a)
newRingIO cap val = Ring cap <$> V.generateM cap (const (newTVarIO val)) <*> newTVarIO initPos

newRing :: Int -> a -> STM (Ring a)
newRing cap val = Ring cap <$> V.generateM cap (const (newTVar val)) <*> newTVar initPos

ringWrite :: Ring a -> a -> STM ()
ringWrite (Ring cap buf hdVar) val = do
  ix <- stateTVar hdVar (\hd -> let hd'@(Pos ix _) = nextPos cap hd in (ix, hd'))
  writeTVar (buf V.! ix) val

newCursor :: Ring a -> STM (Cursor a, a)
newCursor r = do
  hd@(Pos ix _) <- readTVar (ringHead r)
  v <- newTVar hd
  a <- readTVar (ringBuf r V.! ix)
  pure (Cursor r v, a)

cursorRead :: Cursor a -> STM (Maybe a)
cursorRead (Cursor r hd) = undefined

cursorAdvance :: Cursor a -> STM Int
cursorAdvance = undefined

cursorAdvanceRead :: Cursor a -> STM (Int, a)
cursorAdvanceRead = undefined

cursorConsume :: Cursor a -> IO (Seq (Int, a))
cursorConsume = undefined
