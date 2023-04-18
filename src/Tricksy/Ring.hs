module Tricksy.Ring
  ( Ring
  , ringCap
  , newRingIO
  , newRing
  , ringWrite
  , Cursor
  , newCursor
  , newCursorIO
  , cursorRead
  , cursorAdvance
  , cursorAdvanceRead
  , cursorConsume
  ) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO, newTVar, readTVar, stateTVar, writeTVar, readTVarIO)
import Control.Concurrent.STM (STM)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Sequence (Seq)

data Pos = Pos
  { posIndex :: !Int
  , posGen :: !Int
  } deriving stock (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 0 0

nextPos :: Int -> Pos -> Pos
nextPos cap (Pos ix gen) =
  let ix' = ix + 1
  in if ix' == cap
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

ringWrite :: a -> Ring a -> STM ()
ringWrite val (Ring cap buf hdVar) = do
  ix <- stateTVar hdVar (\hd -> let hd'@(Pos ix _) = nextPos cap hd in (ix, hd'))
  writeTVar (buf V.! ix) val

newCursorIO :: Ring a -> IO (Cursor a)
newCursorIO r = fmap (Cursor r) (readTVarIO (ringHead r) >>= newTVarIO)

newCursor :: Ring a -> STM (Cursor a)
newCursor r = fmap (Cursor r) (readTVar (ringHead r) >>= newTVar)

cursorRead :: Cursor a -> STM (Maybe a)
cursorRead (Cursor r hd) = undefined

cursorAdvance :: Cursor a -> STM Int
cursorAdvance = undefined

cursorAdvanceRead :: Cursor a -> STM (Int, a)
cursorAdvanceRead = undefined

cursorConsume :: Cursor a -> IO (Seq (Int, a))
cursorConsume = undefined
