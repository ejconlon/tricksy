module Tricksy.Internal where

import Control.Applicative (liftA2)
import Control.Concurrent.Async (Async, cancel, wait)
import Control.Concurrent.STM (STM, atomically, modifyTVar', newEmptyTMVarIO, retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Exception (catchJust, finally, mask)
import Control.Monad (ap, void, when)
import Data.Bifunctor (first)
import Data.Foldable (for_, toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import System.IO.Error (isEOFError)
import Tricksy.Active (Active (..), ActiveVar, deactivateVar, deactivateVarIO, newActiveVarIO, readActiveVar, readActiveVarIO)
import Tricksy.ActiveScope (scopedActive)
import Tricksy.Cache (CacheHandler)
import Tricksy.Scope (Scope, scoped, spawnAsync)
import Tricksy.Time (MonoTime (..), TimeDelta (..), addMonoTime, currentMonoTime, threadDelayDelta)

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

instance Applicative Events where
  pure a = Events (\cb -> atomically (void (cb a)))
  (<*>) = ap

emptyE :: Events x
emptyE = Events (const (pure ()))

interleaveE :: Events x -> Events x -> Events x
interleaveE el er = Events $ \cb -> scopedActive $ \activeVar scope -> do
  al <- spawnAsync scope (guardedConsume activeVar el cb)
  ar <- spawnAsync scope (guardedConsume activeVar er cb)
  wait al
  wait ar

parallelE :: Foldable f => f (Events x) -> Events x
parallelE es = Events $ \cb -> scopedActive $ \activeVar scope -> do
  as <- traverse (\e -> spawnAsync scope (guardedConsume activeVar e cb)) (toList es)
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

forkSpawner :: ActiveVar -> Scope s -> TMVar a -> TMVar (Async ()) -> (a -> Events b) -> (b -> STM Active) -> IO ()
forkSpawner activeVar scope sourceVar childVar f cb = go
 where
  go = do
    mz <- atomically $ do
      active <- readActiveVar activeVar
      case active of
        ActiveNo -> pure Nothing
        ActiveYes -> do
          a <- takeTMVar sourceVar
          mx <- tryTakeTMVar childVar
          pure (Just (a, mx))
    case mz of
      Nothing -> pure ()
      Just (a, mx) -> do
        for_ mx cancel
        asy <- spawnAsync scope (guardedConsume activeVar (f a) cb)
        atomically (putTMVar childVar asy)
        go

instance Monad Events where
  return = pure
  ea >>= f = Events $ \cb -> do
    sourceVar <- newEmptyTMVarIO
    childVar <- newEmptyTMVarIO
    scopedActive $ \activeVar scope -> do
      source <- spawnAsync scope (guardedConsume activeVar ea (\a -> ActiveYes <$ putTMVar sourceVar a))
      spawner <- spawnAsync scope (forkSpawner activeVar scope sourceVar childVar f cb)
      wait source
      wait spawner
      mchild <- atomically (tryTakeTMVar childVar)
      maybe (pure ()) wait mchild

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

-- rawBuffer :: (b -> Events a) -> b -> (a -> STM ()) -> Events a
-- rawBuffer f b w = Events (\cb -> consumeE (f b) (\a -> w a *> cb a))

capSnoc :: Int -> TVar (Seq a) -> a -> STM ()
capSnoc cap v a =
  modifyTVar' v $ \case
    Empty -> Empty :|> a
    s@(_ :<| t) -> (if Seq.length s >= cap then t else s) :|> a

-- | Fulfills the role of fix
bufferE :: Int -> (STM (Seq a) -> Events a) -> Events a
bufferE cap f = Events $ \cb -> do
  aVar <- newTVarIO Seq.empty
  consumeE (f (readTVar aVar)) (\a -> capSnoc cap aVar a *> cb a)

callZipWith :: (a -> b -> c) -> TVar (Maybe a) -> TVar (Maybe b) -> (c -> STM Active) -> a -> STM Active
callZipWith f aVar bVar cb a = do
  void (writeTVar aVar (Just a))
  mb <- readTVar bVar
  case mb of
    Nothing -> pure ActiveYes
    Just b -> cb (f a b)

zipWithE :: (a -> b -> c) -> Events a -> Events b -> Events c
zipWithE f ea eb = Events $ \cb -> do
  aVar <- newTVarIO Nothing
  bVar <- newTVarIO Nothing
  let cba = callZipWith f aVar bVar cb
      cbb = callZipWith (flip f) bVar aVar cb
  scopedActive $ \activeVar scope -> do
    aa <- spawnAsync scope (guardedConsume activeVar ea cba)
    ab <- spawnAsync scope (guardedConsume activeVar eb cbb)
    wait aa
    wait ab

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

mappendE :: Monoid a => Events a -> Events a
mappendE = scanE (flip (<>)) mempty

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

newtype Alloc x = Alloc {unAlloc :: IO (x, Maybe (IO ()))}

instance Functor Alloc where
  fmap f (Alloc m) = Alloc (fmap (first f) m)

data CacheBehaviorSpec a where
  CacheBehaviorSpec :: !TimeDelta -> !(CacheHandler z a) -> !(IO z) -> CacheBehaviorSpec a

instance Functor CacheBehaviorSpec where
  fmap f (CacheBehaviorSpec ttl han act) = CacheBehaviorSpec ttl (fmap f . han) act

newtype CacheBehavior a = CacheBehavior {unCacheBehavior :: Alloc (CacheBehaviorSpec a)}

instance Functor CacheBehavior where
  fmap f = CacheBehavior . fmap (fmap f) . unCacheBehavior

data HoldBehaviorSpec a = HoldBehaviorSpec
  { hbsStart :: !a
  , hbsEvents :: !(Events a)
  }
  deriving stock (Functor)

newtype HoldBehavior a = HoldBehavior {unHoldBehavior :: Alloc (HoldBehaviorSpec a)}

instance Functor HoldBehavior where
  fmap f = HoldBehavior . fmap (fmap f) . unHoldBehavior

-- applyWithH :: (a -> b -> c) -> HoldBehavior a -> HoldBehavior b -> HoldBehavior c
-- applyWithH f (HoldBehavior sa ea ra) (HoldBehavior sb eb rb) = HoldBehavior (f sa sb) (zipWithE f ea eb) rx
--  where
--   rx = maybe rb (\a -> maybe ra (Just . finally a) rb) ra

-- applyH :: HoldBehavior (a -> b) -> HoldBehavior a -> HoldBehavior b
-- applyH = applyWithH ($)

data MergeBehavior a where
  MergeBehavior :: (x -> y -> a) -> Behavior x -> Behavior y -> MergeBehavior a

instance Functor MergeBehavior where
  fmap f (MergeBehavior g bx by) = MergeBehavior (\x y -> f (g x y)) bx by

data Behavior a
  = BehaviorPure !a
  | BehaviorCache !(CacheBehavior a)
  | BehaviorHold !(HoldBehavior a)
  | BehaviorMerge (MergeBehavior a)
  deriving stock (Functor)

instance Applicative Behavior where
  pure = BehaviorPure
  liftA2 f bx by = BehaviorMerge (MergeBehavior f bx by)

cacheB :: TimeDelta -> CacheHandler z a -> IO z -> Behavior a
cacheB ttl han act = BehaviorCache (CacheBehavior (Alloc (pure (CacheBehaviorSpec ttl han act, Nothing))))

explicitCacheB :: CacheBehavior a -> Behavior a
explicitCacheB = BehaviorCache

holdB :: a -> Events a -> Behavior a
holdB start e = BehaviorHold (HoldBehavior (Alloc (pure (HoldBehaviorSpec start e, Nothing))))

explicitHoldB :: HoldBehavior a -> Behavior a
explicitHoldB = BehaviorHold

data CacheRef a = CacheRef
  { crActiveVar :: !ActiveVar
  , crAction :: !(STM a)
  }

data HoldRef a = HoldRef
  { hrActiveVar :: !ActiveVar
  , hrCurVar :: !(TVar a)
  , hrAsync :: !(Async ())
  }

data Ref a
  = RefPure !a
  | RefCache !(CacheRef a)
  | RefHold !(HoldRef a)

applyWithB :: (a -> b -> c) -> Behavior a -> Events b -> Events c
applyWithB f b e = Events $ \cb -> do
  scoped $ \scope -> do
    r <- observeB scope b
    consumeE e (\x -> readR r >>= cb . flip f x)
    finalizeR r

applyB :: Behavior (a -> b) -> Events a -> Events b
applyB = applyWithB ($)

observeB :: Scope s -> Behavior a -> IO (Ref a)
observeB scope = res
 where
  res = \case
    BehaviorPure a -> pure (RefPure a)
    BehaviorCache _ -> error "TODO"
    BehaviorHold (HoldBehavior (Alloc mp)) -> do
      activeVar <- newActiveVarIO
      mask $ \restore -> do
        (HoldBehaviorSpec start e, mrel) <- mp
        curVar <- newTVarIO start
        asy <-
          spawnAsync scope $
            finally
              (restore (guardedConsume activeVar e (\a -> ActiveYes <$ writeTVar curVar a)))
              (deactivateVarIO activeVar *> fromMaybe (pure ()) mrel)
        pure (RefHold (HoldRef activeVar curVar asy))
    BehaviorMerge {} -> error "TODO"

readR :: Ref a -> STM a
readR = \case
  RefPure a -> pure a
  RefCache _ -> error "TODO"
  RefHold (HoldRef _ curVar _) -> readTVar curVar

finalizeR :: Ref a -> IO ()
finalizeR = \case
  RefPure _ -> pure ()
  RefCache _ -> error "TODO"
  RefHold (HoldRef activeVar _ asy) -> deactivateVarIO activeVar *> wait asy

-- | Delays the event stream by some 'TimeDelta'.
-- The delay will happen on the consuming thread.
delayE :: TimeDelta -> Events a -> Events a
delayE delta e = Events (\cb -> threadDelayDelta delta *> consumeE e cb)

-- | Try to emit (time, actual delta) with period delta
periodicE :: TimeDelta -> Events (MonoTime, TimeDelta)
periodicE delta = Events (\cb -> currentMonoTime >>= go cb)
 where
  go cb lastEdgeTime@(MonoTime lastEdgeStamp) = do
    let targetEdgeTime@(MonoTime targetEdgeStamp) = addMonoTime lastEdgeTime delta
    MonoTime beforeWaitStamp <- currentMonoTime
    when (beforeWaitStamp < targetEdgeStamp) (threadDelayDelta (TimeDelta (targetEdgeStamp - beforeWaitStamp)))
    MonoTime afterWaitStamp <- currentMonoTime
    active <- atomically (cb (targetEdgeTime, TimeDelta (afterWaitStamp - lastEdgeStamp)))
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go cb targetEdgeTime

-- | Evently spaced times
clockE :: TimeDelta -> Events MonoTime
clockE = fmap fst . periodicE

-- | Actual time deltas since previous tick
tickE :: TimeDelta -> Events TimeDelta
tickE = fmap snd . periodicE

-- | Actual time deltas since first tick
timerE :: TimeDelta -> Events TimeDelta
timerE = mappendE . tickE

produce :: ActiveVar -> TChan a -> (a -> IO Active) -> IO ()
produce activeVar c f = go
 where
  go = do
    ma <- atomically $ do
      ma <- tryReadTChan c
      case ma of
        Just _ -> pure ma
        Nothing -> do
          active <- readActiveVar activeVar
          case active of
            ActiveNo -> pure Nothing
            ActiveYes -> retry
    case ma of
      Nothing -> pure ()
      Just a -> do
        active <- f a
        case active of
          ActiveNo -> deactivateVarIO activeVar
          ActiveYes -> go

-- | Runs the callback on all events in the stream.
runE :: (a -> IO Active) -> Events a -> IO ()
runE f e = do
  c <- newTChanIO
  let sourceCb a = ActiveYes <$ writeTChan c a
  scopedActive $ \activeVar scope -> do
    source <- spawnAsync scope (guardedConsume activeVar e sourceCb)
    sink <- spawnAsync scope (produce activeVar c f)
    wait source
    deactivateVarIO activeVar
    wait sink

-- | Creates an even stream from an IO action. Returning 'Nothing' ends the stream.
repeatMayE :: IO (Maybe a) -> Events a
repeatMayE f = Events go
 where
  go cb = do
    ma <- f
    case ma of
      Nothing -> pure ()
      Just a -> do
        active <- atomically (cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go cb

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
stdinE = repeatMayE (catchJust (\e -> if isEOFError e then Just () else Nothing) (fmap Just getLine) (const (pure Nothing)))

-- | Prints events with timestamps
debugPrintE :: Show a => Events a -> IO ()
debugPrintE = runE $ \a -> do
  m <- currentMonoTime
  print (m, a)
  pure ActiveYes
