module Tricksy.Main
  ( main
  )
where

import Control.Applicative (Alternative (..))
import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.Async (concurrently_, mapConcurrently_)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, retry)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (catchJust, finally, onException)
import Control.Monad (ap, void, when)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Tuple (swap)
import System.IO.Error (isEOFError)
import Tricksy.Time (MonoTime, TimeDelta, currentMonoTime)

main :: IO ()
main = putStrLn "Hello, world!"

data Alive = AliveYes | AliveNo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype Events a = Events {consume :: (a -> IO Alive) -> IO ()}

instance Functor Events where
  fmap f e = Events (\cb -> consume e (cb . f))

data Ap a b = Ap
  { apLeft :: !(Maybe (a -> b))
  , apRight :: !(Maybe a)
  }

writeApLeft :: TVar (Ap a b) -> (a -> b) -> STM (Maybe b)
writeApLeft apVar f = stateTVar apVar $ \(Ap _ ma) ->
  let ap' = Ap (Just f) ma
  in  case ma of
        Nothing -> (Nothing, ap')
        Just a -> (Just (f a), ap')

writeApRight :: TVar (Ap a b) -> a -> STM (Maybe b)
writeApRight apVar a = stateTVar apVar $ \(Ap mf _) ->
  let ap' = Ap mf (Just a)
  in  case mf of
        Nothing -> (Nothing, ap')
        Just f -> (Just (f a), ap')

data ParState b = ParStateHalt | ParStateContinue | ParStateEmit b

callAp :: TVar Alive -> TVar (Ap a b) -> (b -> IO Alive) -> (TVar (Ap a b) -> x -> STM (Maybe b)) -> x -> IO Alive
callAp aliveVar apVar cb write x = do
  ps <- atomically $ do
    alive <- readTVar aliveVar
    case alive of
      AliveNo -> pure ParStateHalt
      AliveYes -> fmap (maybe ParStateContinue ParStateEmit) (write apVar x)
  case ps of
    ParStateHalt -> pure AliveNo
    ParStateContinue -> pure AliveYes
    ParStateEmit b -> do
      stillAlive <- cb b
      case stillAlive of
        AliveNo -> atomically (writeTVar aliveVar AliveNo)
        AliveYes -> pure ()
      pure stillAlive

instance Applicative Events where
  pure a = Events (\cb -> void (cb a))
  el <*> er = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    apVar <- newTVarIO (Ap Nothing Nothing)
    let cbl = callAp aliveVar apVar cb writeApLeft
        cbr = callAp aliveVar apVar cb writeApRight
    concurrently_ (consume el cbl) (consume er cbr)

callAlt :: TVar Alive -> (b -> IO Alive) -> b -> IO Alive
callAlt aliveVar cb b = do
  alive <- readTVarIO aliveVar
  case alive of
    AliveNo -> pure AliveNo
    AliveYes -> do
      stillAlive <- cb b
      case stillAlive of
        AliveNo -> atomically (writeTVar aliveVar AliveNo)
        AliveYes -> pure ()
      pure stillAlive

instance Alternative Events where
  empty = Events (const (pure ()))
  el <|> er = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    let cb' = callAlt aliveVar cb
    concurrently_ (consume el cb') (consume er cb')

parallel :: Foldable f => f (Events x) -> Events x
parallel es = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  let cb' = callAlt aliveVar cb
  mapConcurrently_ (`consume` cb') es

callAndThen :: TVar Alive -> (b -> IO Alive) -> b -> IO Alive
callAndThen aliveVar cb b = do
  stillAlive <- cb b
  case stillAlive of
    AliveNo -> atomically (writeTVar aliveVar AliveNo)
    AliveYes -> pure ()
  pure stillAlive

andThen :: Events x -> Events x -> Events x
andThen e1 e2 = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  consume e1 (callAndThen aliveVar cb)
  stillAlive <- readTVarIO aliveVar
  case stillAlive of
    AliveNo -> pure ()
    AliveYes -> consume e2 cb

sequential :: Foldable f => f (Events x) -> Events x
sequential es = res
 where
  res = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    let cb' = callAndThen aliveVar cb
    case toList es of
      [] -> pure ()
      z : zs -> go aliveVar z zs cb'
  go aliveVar z zs cb' = do
    consume z cb'
    stillAlive <- readTVarIO aliveVar
    case stillAlive of
      AliveNo -> pure ()
      AliveYes ->
        case zs of
          [] -> pure ()
          z' : zs' -> go aliveVar z' zs' cb'

instance Monad Events where
  return = pure
  ea >>= f = Events $ \cb -> do
    undefined

once :: IO a -> Events a
once act = Events (\cb -> void (act >>= cb))

repeatedly :: IO a -> Events a
repeatedly act = Events go
 where
  go cb = do
    a <- act
    alive <- cb a
    case alive of
      AliveNo -> pure ()
      AliveYes -> go cb

each :: Foldable f => f a -> Events a
each fa = Events (go (toList fa))
 where
  go as cb =
    case as of
      [] -> pure ()
      a : as' -> do
        alive <- cb a
        case alive of
          AliveNo -> pure ()
          AliveYes -> go as' cb

zipE :: Events a -> Events b -> Events (a, b)
zipE = zipWithE (,)

zipWithE :: (a -> b -> c) -> Events a -> Events b -> Events c
zipWithE f ea eb = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  aVar <- newEmptyTMVarIO
  bVar <- newEmptyTMVarIO
  let cba = callZipWith f aliveVar aVar bVar cb
      cbb = callZipWith (flip f) aliveVar bVar aVar cb
  concurrently_ (consume ea cba) (consume eb cbb)

callZipWith :: (a -> b -> c) -> TVar Alive -> TMVar a -> TMVar b -> (c -> IO Alive) -> a -> IO Alive
callZipWith f aliveVar aVar bVar cb a = do
  ps <- atomically $ do
    alive <- readTVar aliveVar
    case alive of
      AliveNo -> pure ParStateHalt
      AliveYes -> do
        putTMVar aVar a
        mb <- tryTakeTMVar bVar
        case mb of
          Nothing ->
            pure ParStateContinue
          Just b -> do
            _ <- takeTMVar aVar
            pure (ParStateEmit (f a b))
  case ps of
    ParStateHalt -> pure AliveNo
    ParStateContinue -> pure AliveYes
    ParStateEmit c -> do
      stillAlive <- cb c
      case stillAlive of
        AliveNo -> atomically (writeTVar aliveVar AliveNo)
        AliveYes -> pure ()
      pure stillAlive

scanE :: (a -> b -> b) -> b -> Events a -> Events b
scanE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consume e (\a -> atomically (stateTVar bVar (\b -> let b' = f a b in (b', b'))) >>= cb)

scanMayE :: (a -> b -> Maybe b) -> b -> Events a -> Events b
scanMayE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consume e $ \a -> do
    mb <- atomically (stateTVar bVar (\b -> maybe (Nothing, b) (\b' -> (Just b', b')) (f a b)))
    maybe (pure AliveYes) cb mb

mapMayE :: (a -> Maybe b) -> Events a -> Events b
mapMayE f e = Events (\cb -> consume e (maybe (pure AliveYes) cb . f))

accumE :: (a -> s -> (b, s)) -> s -> Events a -> Events b
accumE f s0 e = Events $ \cb -> do
  sVar <- newTVarIO s0
  consume e (\a -> atomically (stateTVar sVar (f a)) >>= cb)

accumMayE :: (a -> s -> Maybe (b, s)) -> s -> Events a -> Events b
accumMayE f s0 e = Events $ \cb -> do
  sVar <- newTVarIO s0
  consume e $ \a -> do
    mb <- atomically (stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f a s)))
    maybe (pure AliveYes) cb mb

filterE :: (a -> Bool) -> Events a -> Events a
filterE f e = Events (\cb -> consume e (\a -> if f a then cb a else pure AliveYes))

filterJustE :: Events (Maybe a) -> Events a
filterJustE e = Events (consume e . maybe (pure AliveYes))

leftE :: Events (Either a b) -> Events a
leftE e = Events (\cb -> consume e (either cb (const (pure AliveYes))))

rightE :: Events (Either a b) -> Events b
rightE e = Events (consume e . either (const (pure AliveYes)))

-- iterateE :: (a -> a) -> a -> Events b -> Events a
-- iterateE = undefined

-- withIterateE :: (a -> a) -> a -> Events b -> Events (a, b)
-- withIterateE = undefined

sumE :: Num a => Events a -> Events a
sumE = scanE (+) 0

productE :: Num a => Events a -> Events a
productE = scanE (*) 1

countE :: Events a -> Events Int
countE = scanE (const (+ 1)) 0

-- withCountE :: Events a -> Events (Int, a)
-- withCountE = undefined

appendE :: Monoid a => Events a -> Events a
appendE = scanE (flip (<>)) mempty

foldMapE :: Monoid b => (a -> b) -> Events a -> Events b
foldMapE f = scanE (flip (<>) . f) mempty

-- takeE :: Int -> Events a -> Events a
-- takeE = undefined

-- dropE :: Int -> Events a -> Events a
-- dropE = undefined

-- takeWhileE :: (a -> Bool) -> Events a -> Events a
-- takeWhileE = undefined

-- dropWhileE :: (a -> Bool) -> Events a -> Events a
-- dropWhileE = undefined

-- cycleE :: [a] -> Events b -> Events a
-- cycleE = undefined

-- seqAtE :: Seq a -> Events Int -> Events a
-- seqAtE = undefined

-- mapAtE :: Ord k => Map k v -> Events k -> Events v
-- mapAtE = undefined

data Hold a = Hold
  { holdStart :: !a
  , holdEvents :: !(Events a)
  , holdRelease :: !(Maybe (IO ()))
  }
  deriving stock (Functor)

data Behavior a
  = BehaviorPure !a
  | BehaviorHold !(IO (Hold a))
  deriving stock (Functor)

instance Applicative Behavior where
  pure = BehaviorPure
  (<*>) = undefined

holdB :: IO (Hold a) -> Behavior a
holdB = BehaviorHold

edgeE :: Behavior a -> Events a
edgeE = \case
  BehaviorPure a -> pure a
  BehaviorHold mh -> Events $ \cb -> do
    Hold start e mrel <- mh
    let go = cb start >>= \case AliveNo -> pure (); AliveYes -> consume e cb
    maybe go (finally go) mrel

data HoldRef a = HoldRef
  { hrAliveVar :: !(TVar Alive)
  , hrCurVar :: !(TVar a)
  , hrTid :: !ThreadId
  }

data Ref a
  = RefPure !a
  | RefHold !(HoldRef a)

guardedWrite :: TVar Alive -> TVar a -> a -> IO Alive
guardedWrite aliveVar curVar val = atomically $ do
  alive <- readTVar aliveVar
  case alive of
    AliveYes -> writeTVar curVar val
    AliveNo -> pure ()
  pure alive

guardedRelease :: TVar Alive -> Maybe (IO ()) -> IO ()
guardedRelease aliveVar mrel =
  case mrel of
    Nothing -> atomically (writeTVar aliveVar AliveNo)
    Just rel -> do
      cleanup <- atomically $ stateTVar aliveVar $ \alive ->
        case alive of
          AliveYes -> (True, AliveNo)
          AliveNo -> (False, alive)
      when cleanup rel

refB :: Behavior a -> IO (Ref a)
refB = res
 where
  res = \case
    BehaviorPure a -> pure (RefPure a)
    BehaviorHold mh -> do
      aliveVar <- newTVarIO AliveYes
      -- TODO need to mask to ensure release always happens
      Hold start e mrel <- mh
      goHold aliveVar start e mrel
  goHold aliveVar start e mrel = do
    curVar <- newTVarIO start
    tid <- forkFinally (consume e (guardedWrite aliveVar curVar)) (const (guardedRelease aliveVar mrel))
    pure (RefHold (HoldRef aliveVar curVar tid))

observeR :: Ref a -> STM a
observeR = \case
  RefPure a -> pure a
  RefHold (HoldRef _ curVar _) -> readTVar curVar

disposeR :: Ref a -> IO ()
disposeR = \case
  RefPure _ -> pure ()
  RefHold (HoldRef _ _ tid) -> killThread tid

clockE :: TimeDelta -> Events MonoTime
clockE = undefined

pulseE :: TimeDelta -> Events ()
pulseE = undefined

tickE :: TimeDelta -> Events TimeDelta
tickE = undefined

timerE :: TimeDelta -> Events TimeDelta
timerE = undefined

-- periodic :: TimeDelta -> IO Alive -> IO ()
-- periodic delta act = undefined

channelE :: TVar Alive -> TChan a -> Events a
channelE aliveVar chanVar = Events go
 where
  go cb = do
    ma <- atomically $ do
      alive <- readTVar aliveVar
      case alive of
        AliveNo -> pure Nothing
        AliveYes -> fmap Just (readTChan chanVar)
    case ma of
      Nothing -> pure ()
      Just a -> do
        alive <- cb a
        case alive of
          AliveNo -> atomically (writeTVar aliveVar AliveNo)
          AliveYes -> go cb

-- | Reads to EOF
stdinE :: Events String
stdinE = Events go
 where
  go cb = do
    ms <- catchJust (\e -> if isEOFError e then Just () else Nothing) (fmap Just getLine) (const (pure Nothing))
    case ms of
      Nothing -> pure ()
      Just s -> do
        alive <- cb s
        case alive of
          AliveNo -> pure ()
          AliveYes -> go cb
