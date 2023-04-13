module Tricksy.Internal
  ( Events
  , consumeE
  , parallelE
  , andThenE
  , sequentialE
  , liftE
  , repeatE
  , eachE
  , zipWithE
  , zipE
  , unfoldE
  , mapMayE
  , scanE
  , scanMayE
  , accumE
  , accumMayE
  , filterE
  )
where

import Control.Applicative (Alternative (..))
import Control.Concurrent.Async (Async, async, cancel, concurrently_, mapConcurrently_, pollSTM, withAsync, wait)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, retry)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (catchJust, finally, mask, throwIO)
import Control.Monad (ap, void, when, (>=>))
import Data.Bifunctor (first)
import Data.Foldable (toList, traverse_, for_)
import System.IO.Error (isEOFError)
import Tricksy.Active (Active (..), ActiveVar, deactivateVar, deactivateVarIO, newActiveVarIO, readActiveVar, readActiveVarIO)
import Tricksy.Time (MonoTime, TimeDelta, addMonoTime, currentMonoTime, diffMonoTime, threadDelayDelta)
import Tricksy.ActiveScope (scopedActive, spawnActive)

-- | Event producer - takes a consumer callback and pushes events through.
-- When the consumer callback signals not active, the producer should stop pushing.
newtype Events a = Events {consumeE :: (a -> STM Active) -> IO ()}

instance Functor Events where
  fmap f e = Events (\cb -> consumeE e (cb . f))

-- | Wrap a callback to check producer liveness before invoking callback and
-- to update the var after invoking it.
guardedCall :: ActiveVar -> (a -> STM Active) -> a -> STM Active
guardedCall activeVar cb a = do
  active <- readActiveVar activeVar
  case active of
    ActiveNo -> pure ActiveNo
    ActiveYes -> do
      stillActive <- cb a
      case stillActive of
        ActiveNo -> deactivateVar activeVar
        ActiveYes -> pure ()
      pure stillActive

-- | Consume while checking producer liveness.
guardedConsume :: ActiveVar -> Events a -> (a -> STM Active) -> IO ()
guardedConsume activeVar e cb = consumeE e (guardedCall activeVar cb)

-- | Consume async with thread killed when producer dies.
guardedAsyncConsume :: ActiveVar -> Events a -> (a -> STM Active) -> IO ()
guardedAsyncConsume activeVar e cb = do
  withAsync (guardedConsume activeVar e cb) $ \asy -> do
    (shouldCancel, merr) <- atomically $ do
      mres <- pollSTM asy
      case mres of
        Nothing -> do
          active <- readActiveVar activeVar
          case active of
            ActiveYes -> retry
            ActiveNo -> pure (True, Nothing)
        Just res -> do
          case res of
            Left err -> do
              deactivateVar activeVar
              pure (False, Just err)
            Right _ -> pure (False, Nothing)
    let cleanup = when shouldCancel (cancel asy)
    maybe cleanup (finally cleanup . throwIO) merr

instance Applicative Events where
  pure a = Events (\cb -> atomically (void (cb a)))
  (<*>) = ap

instance Alternative Events where
  empty = Events (const (pure ()))
  el <|> er = Events $ \cb -> scopedActive $ \activeVar scope -> do
    al <- spawnActive activeVar scope (guardedConsume activeVar el cb)
    ar <- spawnActive activeVar scope (guardedConsume activeVar er cb)
    wait al
    wait ar

parallelE :: Foldable f => f (Events x) -> Events x
parallelE es = Events $ \cb -> scopedActive $ \activeVar scope -> do
  as <- traverse (\e -> spawnActive activeVar scope (guardedConsume activeVar e cb)) (toList es)
  for_ as wait

andThenE :: Events x -> Events x -> Events x
andThenE e1 e2 = Events $ \cb -> do
  activeVar <- newActiveVarIO
  consumeE e1 (guardedCall activeVar cb)
  stillActive <- readActiveVarIO activeVar
  case stillActive of
    ActiveNo -> pure ()
    ActiveYes -> consumeE e2 cb

sequentialE :: Foldable f => f (Events x) -> Events x
sequentialE es = res
 where
  res = Events $ \cb -> do
    activeVar <- newActiveVarIO
    let cbg = guardedCall activeVar cb
    case toList es of
      [] -> pure ()
      z : zs -> go activeVar z zs cbg
  go activeVar z zs cbg = do
    consumeE z cbg
    stillActive <- readActiveVarIO activeVar
    case stillActive of
      ActiveNo -> pure ()
      ActiveYes ->
        case zs of
          [] -> pure ()
          z' : zs' -> go activeVar z' zs' cbg

instance Monad Events where
  return = pure
  (>>=) = undefined

-- ea >>= f = Events go where
--   go cb = do
--     genVar <- newTVarIO (0 :: Int)
--     asyncVar <- newEmptyTMVarIO
--     activeVar <- newActiveVarIO
--     finally (goOuter genVar asyncVar activeVar cb) (goClean asyncVar)
--   goOuter genVar asyncVar activeVar cb = consumeE ea $ \a -> do
--     gen <- atomically (stateTVar genVar (\s -> (s, s + 1)))
--     goSpawn genVar asyncVar activeVar cb gen (f a)
--     readActiveVarIO activeVar
--   goClean asyncVar = atomically (tryTakeTMVar asyncVar) >>= traverse_ cancel
--   goSpawn genVar asyncVar activeVar cb gen eb = mask $ \restore -> do
--       newa <- async (restore (guardedConsume activeVar eb cb))
--       molda <- atomically $ do
--         molda <- tryTakeTMVar asyncVar
--         putTMVar asyncVar newa
--         pure molda
--       maybe (pure ()) cancel molda

liftE :: IO a -> Events a
liftE act = Events (\cb -> act >>= atomically . void . cb)

repeatE :: IO a -> Events a
repeatE act = Events go
 where
  go cb = do
    a <- act
    active <- atomically (cb a)
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go cb

eachE :: Foldable f => f a -> Events a
eachE fa = Events (go (toList fa))
 where
  go as cb =
    case as of
      [] -> pure ()
      a : as' -> do
        active <- atomically (cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go as' cb

callZipWith :: (a -> b -> c) -> TMVar a -> TMVar b -> (c -> STM Active) -> a -> STM Active
callZipWith f aVar bVar cb a = do
  putTMVar aVar a
  mb <- tryTakeTMVar bVar
  case mb of
    Nothing ->
      pure ActiveYes
    Just b -> do
      _ <- takeTMVar aVar
      cb (f a b)

zipWithE :: (a -> b -> c) -> Events a -> Events b -> Events c
zipWithE f ea eb = Events $ \cb -> do
  activeVar <- newActiveVarIO
  aVar <- newEmptyTMVarIO
  bVar <- newEmptyTMVarIO
  let cba = callZipWith f aVar bVar cb
      cbb = callZipWith (flip f) bVar aVar cb
  concurrently_ (guardedAsyncConsume activeVar ea cba) (guardedAsyncConsume activeVar eb cbb)

zipE :: Events a -> Events b -> Events (a, b)
zipE = zipWithE (,)

unfoldE :: (s -> Maybe (a, s)) -> s -> Events a
unfoldE f s0 = Events (\cb -> newTVarIO s0 >>= go cb)
 where
  go cb sVar = do
    active <- atomically $ do
      ma <- stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f s))
      maybe (pure ActiveNo) cb ma
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go cb sVar

mapMayE :: (a -> Maybe b) -> Events a -> Events b
mapMayE f e = Events (\cb -> consumeE e (maybe (pure ActiveYes) cb . f))

scanE :: (a -> b -> b) -> b -> Events a -> Events b
scanE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consumeE e (\a -> stateTVar bVar (\b -> let b' = f a b in (b', b')) >>= cb)

scanMayE :: (a -> b -> Maybe b) -> b -> Events a -> Events b
scanMayE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consumeE e $ \a -> do
    mb <- stateTVar bVar (\b -> maybe (Nothing, b) (\b' -> (Just b', b')) (f a b))
    maybe (pure ActiveYes) cb mb

accumE :: (a -> s -> (b, s)) -> s -> Events a -> Events b
accumE f s0 e = Events $ \cb -> do
  sVar <- newTVarIO s0
  consumeE e (\a -> stateTVar sVar (f a) >>= cb)

accumMayE :: (a -> s -> (Maybe b, s)) -> s -> Events a -> Events b
accumMayE f s0 e = Events $ \cb -> do
  sVar <- newTVarIO s0
  consumeE e $ \a -> do
    mb <- stateTVar sVar (f a)
    maybe (pure ActiveYes) cb mb

filterE :: (a -> Bool) -> Events a -> Events a
filterE f e = Events (\cb -> consumeE e (\a -> if f a then cb a else pure ActiveYes))

filterJustE :: Events (Maybe a) -> Events a
filterJustE e = Events (consumeE e . maybe (pure ActiveYes))

leftE :: Events (Either a b) -> Events a
leftE = mapMayE (either Just (const Nothing))

rightE :: Events (Either a b) -> Events b
rightE = mapMayE (either (const Nothing) Just)

sumE :: Num a => Events a -> Events a
sumE = scanE (+) 0

productE :: Num a => Events a -> Events a
productE = scanE (*) 1

countE :: Events a -> Events Int
countE = scanE (const (+ 1)) 0

enumerateE :: Events a -> Events (Int, a)
enumerateE = accumE (\a i -> ((i, a), i + 1)) 0

appendE :: Monoid a => Events a -> Events a
appendE = scanE (flip (<>)) mempty

foldMapE :: Monoid b => (a -> b) -> Events a -> Events b
foldMapE f = scanE (flip (<>) . f) mempty

takeE :: Int -> Events a -> Events a
takeE n0 e = Events $ \cb -> do
  nVar <- newTVarIO n0
  consumeE e $ \a -> do
    taking <- stateTVar nVar (\n -> if n > 0 then (True, n - 1) else (False, n))
    if taking then cb a else pure ActiveNo

dropE :: Int -> Events a -> Events a
dropE = accumMayE (\a n -> if n > 0 then (Nothing, n - 1) else (Just a, n))

takeWhileE :: (a -> Bool) -> Events a -> Events a
takeWhileE f e = Events (\cb -> consumeE e (\a -> if f a then cb a else pure ActiveNo))

dropWhileE :: (a -> Bool) -> Events a -> Events a
dropWhileE f = accumMayE (\a dropping -> if dropping && f a then (Nothing, dropping) else (Just a, False)) True

cycleE :: Foldable f => f a -> Events a
cycleE fa = Events (\cb -> let as0 = toList fa in case as0 of [] -> pure (); _ -> go as0 as0 cb)
 where
  go as0 as cb =
    case as of
      [] -> go as0 as0 cb
      a : as' -> do
        active <- atomically (cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go as0 as' cb

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
  BehaviorHold mh -> Events $ \cb -> mask $ \restore -> do
    Hold start e mrel <- mh
    let go = restore $ do
          active <- atomically (cb start)
          case active of
            ActiveNo -> pure ()
            ActiveYes -> consumeE e cb
    maybe go (finally go) mrel

data HoldRef a = HoldRef
  { hrCurVar :: !(TVar a)
  , hrAsync :: !(Async ())
  }

data Ref a
  = RefPure !a
  | RefHold !(HoldRef a)

observeB :: Behavior a -> IO (Ref a)
observeB = res
 where
  res = \case
    BehaviorPure a -> pure (RefPure a)
    BehaviorHold mh -> mask $ \restore -> do
      Hold start e mrel <- mh
      curVar <- newTVarIO start
      let bgInner = restore (consumeE e (\a -> ActiveYes <$ writeTVar curVar a))
          bgOuter = maybe bgInner (finally bgInner) mrel
      withAsync bgOuter (pure . RefHold . HoldRef curVar)

readR :: Ref a -> STM a
readR = \case
  RefPure a -> pure a
  RefHold (HoldRef curVar _) -> readTVar curVar

peekR :: Ref a -> IO a
peekR = \case
  RefPure a -> pure a
  RefHold (HoldRef curVar _) -> readTVarIO curVar

disposeR :: Ref a -> IO ()
disposeR = \case
  RefPure _ -> pure ()
  RefHold (HoldRef _ asy) -> cancel asy

-- | Delays the event stream by some 'TimeDelta'.
-- The delay will happen on the consuming thread.
delayE :: TimeDelta -> Events a -> Events a
delayE delta e = Events (\cb -> threadDelayDelta delta *> consumeE e cb)

periodicE :: TimeDelta -> Events (MonoTime, TimeDelta)
periodicE delta = Events (\cb -> currentMonoTime >>= go cb 0)
 where
  go cb accDelta lastEdgeTime = do
    let targetEdgeTime = addMonoTime lastEdgeTime delta
    curTime <- currentMonoTime
    let nextAccDelta = accDelta + delta
    case diffMonoTime targetEdgeTime curTime of
      Nothing -> go cb nextAccDelta targetEdgeTime
      Just targetDelta -> do
        threadDelayDelta targetDelta
        active <- atomically (cb (targetEdgeTime, nextAccDelta))
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go cb 0 targetEdgeTime

clockE :: TimeDelta -> Events MonoTime
clockE = fmap fst . periodicE

pulseE :: TimeDelta -> Events ()
pulseE = void . periodicE

tickE :: TimeDelta -> Events TimeDelta
tickE = fmap snd . periodicE

timerE :: TimeDelta -> Events TimeDelta
timerE = appendE . tickE

-- | Events from a (closable) channel
channelE :: ActiveVar -> TChan a -> Events a
channelE activeVar chanVar = Events go
 where
  go cb = do
    active <- atomically $ do
      active <- readActiveVar activeVar
      case active of
        ActiveNo -> do
          deactivateVar activeVar
          pure active
        ActiveYes -> do
          a <- readTChan chanVar
          stillActive <- cb a
          case stillActive of
            ActiveNo -> deactivateVar activeVar
            ActiveYes -> pure ()
          pure stillActive
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go cb

-- | Reads to EOF
stdinE :: Events String
stdinE = Events go
 where
  go cb = do
    ms <- catchJust (\e -> if isEOFError e then Just () else Nothing) (fmap Just getLine) (const (pure Nothing))
    case ms of
      Nothing -> pure ()
      Just s -> do
        active <- atomically (cb s)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go cb
