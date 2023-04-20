module Tricksy.Ring
  ( Ring
  , ringCap
  , newRingIO
  , newRing
  , ringWrite
  , Next (..)
  , Cursor
  , newCursor
  , newCursorIO
  , cursorNext
  )
where

import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Data.Vector (Vector)
import Data.Vector qualified as V

data Pos = Pos
  { posGen :: !Int
  , posIndex :: !Int
  }
  deriving stock (Eq, Ord, Show)

initPos :: Pos
initPos = Pos 0 0

nextPos :: Int -> Pos -> Pos
nextPos cap (Pos gen ix) =
  let ix' = ix + 1
  in  if ix' == cap
        then Pos (gen + 1) 0
        else Pos gen ix'

readLimit :: Pos -> Maybe Pos
readLimit (Pos gen ix) = do
  if gen == 0
    then if ix == 0 then Nothing else Just (Pos 0 0)
    else Just (Pos (gen - 1) ix)

elemDiff :: Int -> Pos -> Pos -> Int
elemDiff cap (Pos endGen endIx) (Pos startGen startIx) =
  cap * (endGen - startGen) + endIx - startIx

data Ring a = Ring
  { ringCap :: !Int
  , ringBuf :: !(Vector (TVar a))
  , ringWriteHead :: !(TVar Pos)
  }

data Next a = Next {nextDropped :: !Int, nextValue :: !a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Cursor a = Cursor
  { cursorRing :: !(Ring a)
  , cursorReadHead :: !(TVar Pos)
  }

uninit :: a
uninit = error "Evaluated uninitialized ring element"

newRingIO :: Int -> IO (Ring a)
newRingIO cap = Ring cap <$> V.generateM cap (const (newTVarIO uninit)) <*> newTVarIO initPos

newRing :: Int -> STM (Ring a)
newRing cap = Ring cap <$> V.generateM cap (const (newTVar uninit)) <*> newTVar initPos

ringWrite :: Ring a -> a -> STM ()
ringWrite (Ring cap buf hdVar) !val = do
  ix <- stateTVar hdVar (\hd -> let hd'@(Pos _ ix) = nextPos cap hd in (ix, hd'))
  writeTVar (buf V.! ix) val

newCursor :: Ring a -> STM (Cursor a)
newCursor r = do
  h <- readTVar (ringWriteHead r)
  v <- newTVar h
  pure (Cursor r v)

newCursorIO :: Ring a -> IO (Cursor a)
newCursorIO r = do
  h <- readTVarIO (ringWriteHead r)
  v <- newTVarIO h
  pure (Cursor r v)

cursorNext :: Cursor a -> STM (Next a)
cursorNext (Cursor r rhVar) = do
  rh <- readTVar rhVar
  wh <- readTVar (ringWriteHead r)
  if rh == wh
    then retry
    else case readLimit wh of
      Nothing -> retry
      Just rl -> do
        let (dropped, used) = if rh >= rl then (0, rh) else (elemDiff (ringCap r) rl rh, rl)
            !pos = nextPos (ringCap r) used
        writeTVar rhVar pos
        fmap (Next dropped) (readTVar (ringBuf r V.! posIndex used))
