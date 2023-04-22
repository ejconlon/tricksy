module Tricksy.Internal where

import Control.Applicative (liftA2)
import Control.Concurrent.STM (STM, atomically, modifyTVar', newEmptyTMVarIO, orElse, retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Exception (catchJust)
import Control.Monad (ap, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Foldable (for_, toList)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import System.IO.Error (isEOFError)
import Tricksy.Active (Active (..))
import Tricksy.Barrier (newBarrier, trackBarrier)
import Tricksy.Cache (CacheHandler, runCache)
import Tricksy.Control (Control (..), allocateControl, controlDeactivateIO, controlReadActiveIO, guarded, guardedMay, guarded_, newControl, scopedControl, trackControl)
import Tricksy.Monad (ResM, finallyRegister, runResM, spawnThread, stopThread)
import Tricksy.Ref (Ref (..), viewRef)
import Tricksy.Ring (Next, Ring, cursorNext, newCursor, newRingIO)
import Tricksy.Rw (Rw (..), chanRw, newRingRw, ringRwIO)
import Tricksy.Time (MonoTime (..), TimeDelta (..), addMonoTime, currentMonoTime, threadDelayDelta)

-- | Consumer callback
type Callback m a = Control -> a -> m ()

guardedCall :: Control -> Callback STM a -> a -> STM Active
guardedCall ctl cb a = guarded (controlReadActive ctl) (cb ctl a)

guardedCall_ :: Control -> Callback STM a -> a -> STM ()
guardedCall_ ctl cb a = guarded_ (controlReadActive ctl) (cb ctl a)

atomicCall :: Callback STM a -> Callback IO a
atomicCall cb ctl a = atomically (cb ctl a)

-- | Event producer - takes a consumer callback and pushes events through.
newtype Events a = Events {produceE :: Control -> Callback STM a -> IO ()}

instance Functor Events where
  fmap f e = Events (\ctl cb -> produceE e ctl (\d a -> cb d (f a)))

instance Applicative Events where
  pure a = Events (\ctl cb -> liftIO (atomically (guardedCall_ ctl cb a)))
  (<*>) = ap

emptyE :: Events x
emptyE = Events (\_ _ -> pure ())

interleaveE :: Events x -> Events x -> Events x
interleaveE el er = Events $ \ctl cb -> scopedControl ctl $ do
  void (spawnThread (produceE el ctl cb))
  void (spawnThread (produceE er ctl cb))

parallelE :: Foldable f => f (Events x) -> Events x
parallelE es = Events $ \ctl cb -> scopedControl ctl $ do
  for_ es (\e -> spawnThread (produceE e ctl cb))

andThenE :: Events x -> Events x -> Events x
andThenE e1 e2 = Events $ \ctl cb -> do
  produceE e1 ctl cb
  guarded_ (controlReadActiveIO ctl) (produceE e2 ctl cb)

seriesE :: Foldable f => f (Events x) -> Events x
seriesE = go . toList
 where
  go es = Events $ \c cb -> do
    case es of
      [] -> pure ()
      z : zs -> produceE (z `andThenE` go zs) c cb

genControl :: Control -> TVar Int -> Int -> Control
genControl ctl genVar myGen = ctl {controlReadActive = rd, controlWait = wt}
 where
  rd = do
    curGen <- readTVar genVar
    if curGen == myGen
      then controlReadActive ctl
      else pure ActiveNo
  wtg = do
    curGen <- readTVar genVar
    when (curGen == myGen) retry
  wt = wtg `orElse` controlWait ctl

spawnChild :: Control -> Control -> TMVar a -> TVar Int -> (a -> Events b) -> Callback STM b -> IO ()
spawnChild prodCtl conCtl sourceVar genVar f cb = runResM (go Nothing)
 where
  go mc = do
    mz <- liftIO $ atomically $ do
      guardedMay (controlReadActive conCtl) $ do
        ma <- tryTakeTMVar sourceVar
        case ma of
          Nothing -> do
            parentActive <- controlReadActive prodCtl
            case parentActive of
              ActiveNo -> pure Nothing
              ActiveYes -> retry
          Just a -> do
            g <- readTVar genVar
            pure (Just (a, g))
    case mz of
      Nothing -> pure ()
      Just (a, g) -> do
        let e' = f a
            ctl' = genControl conCtl genVar g
        newCid <- spawnThread (produceE e' ctl' cb)
        case mc of
          Just oldCid -> liftIO (stopThread oldCid)
          Nothing -> pure ()
        go (Just newCid)

parentProduce :: Control -> Control -> TMVar a -> TVar Int -> Events a -> IO ()
parentProduce prodCtl conCtl sourceVar genVar ea =
  trackControl prodCtl $ produceE ea conCtl $ \_ a -> do
    putTMVar sourceVar a
    modifyTVar' genVar succ

instance Monad Events where
  return = pure
  ea >>= f = Events $ \conCtl cb ->
    scopedControl conCtl $ do
      prodCtl <- allocateControl
      sourceVar <- liftIO newEmptyTMVarIO
      genVar <- liftIO (newTVarIO 0)
      void (spawnThread (parentProduce prodCtl conCtl sourceVar genVar ea))
      void (spawnThread (spawnChild prodCtl conCtl sourceVar genVar f cb))

liftE :: IO a -> Events a
liftE act = Events (\ctl cb -> liftIO (guarded_ (controlReadActiveIO ctl) (act >>= atomically . guardedCall_ ctl cb)))

repeatE :: IO a -> Events a
repeatE act = Events (\ctl cb -> liftIO (guarded_ (controlReadActiveIO ctl) (go ctl cb)))
 where
  go ctl cb = do
    a <- act
    active <- atomically (guardedCall ctl cb a)
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go ctl cb

eachE :: Foldable f => f a -> Events a
eachE fa = Events (\ctl cb -> liftIO (go (toList fa) ctl cb))
 where
  go as ctl cb =
    case as of
      [] -> pure ()
      a : as' -> do
        active <- atomically (guardedCall ctl cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go as' ctl cb

capSnoc :: Int -> TVar (Seq a) -> a -> STM ()
capSnoc cap v a =
  modifyTVar' v $ \case
    Empty -> Empty :|> a
    s@(_ :<| t) -> (if Seq.length s >= cap then t else s) :|> a

-- | Fulfills the role of fix... to a certain extent
fixE :: Int -> (STM (Seq a) -> Events a) -> Events a
fixE cap f = Events $ \ctl cb -> do
  aVar <- liftIO (newTVarIO Seq.empty)
  produceE (f (readTVar aVar)) ctl (\d a -> capSnoc cap aVar a *> cb d a)

callZipWith :: (a -> b -> c) -> TVar (Maybe a) -> TVar (Maybe b) -> Callback STM c -> Callback STM a
callZipWith f aVar bVar cb ctl = guardedCall_ ctl $ \ctl' a -> do
  void (writeTVar aVar (Just a))
  mb <- readTVar bVar
  case mb of
    Nothing -> pure ()
    Just b -> cb ctl' (f a b)

zipWithE :: (a -> b -> c) -> Events a -> Events b -> Events c
zipWithE f ea eb = Events $ \ctl cb -> do
  aVar <- liftIO (newTVarIO Nothing)
  bVar <- liftIO (newTVarIO Nothing)
  let cba = callZipWith f aVar bVar cb
      cbb = callZipWith (flip f) bVar aVar cb
  scopedControl ctl $ do
    barrier <- liftIO (atomically (newBarrier 1 (controlDeactivate ctl)))
    void (spawnThread (trackBarrier barrier (produceE ea ctl cba)))
    void (spawnThread (trackBarrier barrier (produceE eb ctl cbb)))

zipE :: Events a -> Events b -> Events (a, b)
zipE = zipWithE (,)

unfoldE :: (s -> Maybe (a, s)) -> s -> Events a
unfoldE f s0 = Events (\ctl cb -> liftIO (newTVarIO s0 >>= go ctl cb))
 where
  go ctl cb sVar = do
    active <- atomically $ do
      ma <- stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f s))
      maybe (pure ActiveNo) (guardedCall ctl cb) ma
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go ctl cb sVar

mapMayE :: (a -> Maybe b) -> Events a -> Events b
mapMayE f e = Events (\ctl cb -> produceE e ctl (\d a -> maybe (pure ()) (cb d) (f a)))

scanE :: (a -> b -> b) -> b -> Events a -> Events b
scanE f b0 e = Events $ \ctl cb -> do
  bVar <- liftIO (newTVarIO b0)
  produceE e ctl (\d a -> stateTVar bVar (\b -> let b' = f a b in (b', b')) >>= cb d)

scanMayE :: (a -> b -> Maybe b) -> b -> Events a -> Events b
scanMayE f b0 e = Events $ \ctl cb -> do
  bVar <- liftIO (newTVarIO b0)
  produceE e ctl $ \d a -> do
    mb <- stateTVar bVar (\b -> maybe (Nothing, b) (\b' -> (Just b', b')) (f a b))
    maybe (pure ()) (cb d) mb

accumE :: (a -> s -> (b, s)) -> s -> Events a -> Events b
accumE f s0 e = Events $ \ctl cb -> do
  sVar <- liftIO (newTVarIO s0)
  produceE e ctl (\d a -> stateTVar sVar (f a) >>= cb d)

accumMayE :: (a -> s -> (Maybe b, s)) -> s -> Events a -> Events b
accumMayE f s0 e = Events $ \ctl cb -> do
  sVar <- liftIO (newTVarIO s0)
  produceE e ctl $ \d a -> do
    mb <- stateTVar sVar (f a)
    maybe (pure ()) (cb d) mb

filterE :: (a -> Bool) -> Events a -> Events a
filterE f e = Events (\ctl cb -> produceE e ctl (\d a -> when (f a) (cb d a)))

filterJustE :: Events (Maybe a) -> Events a
filterJustE e = Events (\ctl cb -> produceE e ctl (maybe (pure ()) . cb))

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
takeE n0 e = Events $ \ctl cb -> do
  nVar <- liftIO (newTVarIO n0)
  produceE e ctl $ \d a -> do
    taking <- stateTVar nVar (\n -> if n > 0 then (True, n - 1) else (False, n))
    when taking (cb d a)

dropE :: Int -> Events a -> Events a
dropE = accumMayE (\a n -> if n > 0 then (Nothing, n - 1) else (Just a, n))

takeWhileE :: (a -> Bool) -> Events a -> Events a
takeWhileE f e = Events (\ctl cb -> produceE e ctl (\d a -> when (f a) (cb d a)))

dropWhileE :: (a -> Bool) -> Events a -> Events a
dropWhileE f = accumMayE (\a dropping -> if dropping && f a then (Nothing, dropping) else (Just a, False)) True

cycleE :: Foldable f => f a -> Events a
cycleE fa = Events (\ctl cb -> let as0 = toList fa in case as0 of [] -> pure (); _ -> liftIO (go as0 as0 ctl cb))
 where
  go as0 as ctl cb =
    case as of
      [] -> go as0 as0 ctl cb
      a : as' -> do
        active <- atomically (guardedCall ctl cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go as0 as' ctl cb

data CacheBehavior a where
  CacheBehavior :: !TimeDelta -> !(CacheHandler z a) -> !(IO z) -> CacheBehavior a

instance Functor CacheBehavior where
  fmap f (CacheBehavior ttl han act) = CacheBehavior ttl (fmap f . han) act

data HoldBehavior a = HoldBehavior
  { hbStart :: !a
  , hbEvents :: !(Events a)
  }
  deriving stock (Functor)

data MergeBehavior a where
  MergeBehavior :: (x -> y -> a) -> Behavior x -> Behavior y -> MergeBehavior a

instance Functor MergeBehavior where
  fmap f (MergeBehavior g bx by) = MergeBehavior (\x y -> f (g x y)) bx by

data Behavior a
  = BehaviorPure !a
  | BehaviorCache !(CacheBehavior a)
  | BehaviorHold !(HoldBehavior a)
  | BehaviorMerge !(MergeBehavior a)
  | BehaviorRes !(ResM (Behavior a))
  deriving stock (Functor)

instance Applicative Behavior where
  pure = BehaviorPure
  liftA2 f bx by = BehaviorMerge (MergeBehavior f bx by)

cacheB :: TimeDelta -> CacheHandler z a -> IO z -> Behavior a
cacheB ttl han act = BehaviorCache (CacheBehavior ttl han act)

explicitCacheB :: CacheBehavior a -> Behavior a
explicitCacheB = BehaviorCache

holdB :: a -> Events a -> Behavior a
holdB start e = BehaviorHold (HoldBehavior start e)

explicitHoldB :: HoldBehavior a -> Behavior a
explicitHoldB = BehaviorHold

resB :: ResM (Behavior a) -> Behavior a
resB = BehaviorRes

rawApplyWithB :: IO (Rw b x) -> (a -> x -> IO c) -> Behavior a -> Events b -> Events c
rawApplyWithB mkRw f b e = Events $ \ctl cb ->
  scopedControl ctl $ do
    r <- observeB b
    let newCb d x = viewRef r >>= \a -> f a x >>= atomically . cb d
    liftIO (internalRwRunE mkRw ctl newCb e)

applyWithB :: Int -> (a -> Next b -> IO c) -> Behavior a -> Events b -> Events c
applyWithB = rawApplyWithB . newRingRw

spawnCacheB :: CacheBehavior a -> ResM (Ref a)
spawnCacheB (CacheBehavior ttl han act) = fmap fst (runCache ttl han act)

spawnHoldB :: HoldBehavior a -> ResM (Ref a)
spawnHoldB (HoldBehavior start e) = do
  ctl <- liftIO newControl
  curVar <- liftIO (newTVarIO start)
  finallyRegister
    (void (spawnThread (produceE e ctl (\_ a -> writeTVar curVar a))))
    (controlDeactivateIO ctl)
  pure (Ref (pure (readTVar curVar)))

spawnMergeB :: MergeBehavior a -> ResM (Ref a)
spawnMergeB (MergeBehavior f b1 b2) =
  case b1 of
    BehaviorPure x -> observeB (fmap (f x) b2)
    BehaviorRes mb -> mb >>= \bx -> spawnMergeB (MergeBehavior f bx b2)
    _ -> case b2 of
      BehaviorPure x -> observeB (fmap (`f` x) b1)
      BehaviorRes mb -> mb >>= \bx -> spawnMergeB (MergeBehavior f b1 bx)
      _ -> do
        r1 <- observeB b1
        r2 <- observeB b2
        pure (liftA2 f r1 r2)

observeB :: Behavior a -> ResM (Ref a)
observeB = \case
  BehaviorPure a -> pure (pure a)
  BehaviorCache cb -> spawnCacheB cb
  BehaviorHold hb -> spawnHoldB hb
  BehaviorMerge mb -> spawnMergeB mb
  BehaviorRes rb -> rb >>= observeB

-- | Delays the event stream by some 'TimeDelta'.
-- The delay will happen on the consuming thread.
delayE :: TimeDelta -> Events a -> Events a
delayE delta e = Events (\ctl cb -> liftIO (threadDelayDelta delta) *> produceE e ctl cb)

-- | Try to emit (time, actual delta) with period delta
periodicE :: TimeDelta -> Events (MonoTime, TimeDelta)
periodicE delta = Events (\ctl cb -> liftIO (currentMonoTime >>= go ctl cb))
 where
  go ctl cb lastEdgeTime@(MonoTime lastEdgeStamp) = do
    let targetEdgeTime@(MonoTime targetEdgeStamp) = addMonoTime lastEdgeTime delta
    MonoTime beforeWaitStamp <- currentMonoTime
    when (beforeWaitStamp < targetEdgeStamp) (threadDelayDelta (TimeDelta (targetEdgeStamp - beforeWaitStamp)))
    MonoTime afterWaitStamp <- currentMonoTime
    let tup = (targetEdgeTime, TimeDelta (afterWaitStamp - lastEdgeStamp))
    active <- atomically (guardedCall ctl cb tup)
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go ctl cb targetEdgeTime

-- | Evently spaced times
clockE :: TimeDelta -> Events MonoTime
clockE = fmap fst . periodicE

-- | Actual time deltas since previous tick
tickE :: TimeDelta -> Events TimeDelta
tickE = fmap snd . periodicE

-- | Actual time deltas since first tick
timerE :: TimeDelta -> Events TimeDelta
timerE = mappendE . tickE

consumeIO :: Control -> Control -> STM a -> Callback IO a -> IO ()
consumeIO prodCtl conCtl rd f = go
 where
  go = do
    ma <-
      atomically $
        guardedMay
          (controlReadActive conCtl)
          (orElse (fmap Just rd) (Nothing <$ controlWait prodCtl))
    case ma of
      Nothing -> pure ()
      Just a -> f conCtl a *> go

internalRwRunE :: IO (Rw a b) -> Control -> Callback IO b -> Events a -> IO ()
internalRwRunE mkRw conCtl f e =
  scopedControl conCtl $ do
    rw <- liftIO mkRw
    prodCtl <- allocateControl
    let sourceCb = const (rwWrite rw)
        sinkFn = rwRead rw
    void (spawnThread (trackControl prodCtl (produceE e conCtl sourceCb)))
    void (spawnThread (consumeIO prodCtl conCtl sinkFn f))

-- | Runs the callback on all events in the stream. Takes a function to
-- create a buffer strategy, so you can choose how you want the producer and
-- consumer to interact.
rwRunE :: IO (Rw a b) -> Callback IO b -> Events a -> IO ()
rwRunE mkRw f e = do
  conCtl <- newControl
  internalRwRunE mkRw conCtl f e

-- | Runs the callback on all events in the stream. Uses a ring buffer to
-- maintain constant space during consumption, so the callback is informed of
-- number of dropped events.
ringRunE :: Int -> Callback IO (Next a) -> Events a -> IO ()
ringRunE cap = rwRunE (newRingIO cap >>= ringRwIO)

-- | Creates an event stream from an IO action. Returning 'Nothing' ends the stream.
repeatMayE :: IO (Maybe a) -> Events a
repeatMayE f = Events (\ctl cb -> liftIO (guarded_ (controlReadActiveIO ctl) (go ctl cb)))
 where
  go ctl cb = do
    ma <- f
    case ma of
      Nothing -> pure ()
      Just a -> do
        active <- atomically (guardedCall ctl cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go ctl cb

repeatTxnE :: STM a -> Events a
repeatTxnE act = Events (\ctl cb -> liftIO (go ctl cb))
 where
  go ctl cb = do
    active <- atomically (guarded (controlReadActive ctl) (act >>= cb ctl))
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go ctl cb

-- | Events from a channel
channelE :: TChan a -> Events a
channelE chanVar = repeatTxnE (readTChan chanVar)

-- | Events from a lock var
lockE :: TMVar a -> Events a
lockE lockVar = repeatTxnE (takeTMVar lockVar)

-- | Events from a ring buffer
-- The first component of tuple is the number of dropped events between invocations
-- (always non-negative). If the consumer keeps up with the producer, it will be 0.
ringE :: Ring a -> Events (Next a)
ringE ring = Events (\ctl cb -> liftIO (goStart ctl cb))
 where
  goStart ctl cb = do
    mc <- atomically $ guardedMay (controlReadActive ctl) $ do
      cur <- newCursor ring
      p <- cursorNext cur
      cb ctl p
      pure (Just cur)
    for_ mc (goRest ctl cb)
  goRest ctl cb cur = do
    active <- atomically (guarded (controlReadActive ctl) (cursorNext cur >>= cb ctl))
    case active of
      ActiveNo -> pure ()
      ActiveYes -> goRest ctl cb cur

-- | Buffers the events stream using the given R/W strategy.
rwBufferE :: IO (Rw a b) -> Events a -> Events b
rwBufferE mkRw e = Events (\ctl cb -> internalRwRunE mkRw ctl (atomicCall cb) e)

-- | Buffers the events stream using a ring buffer with the given capacity.
-- Dropped event counts are added to the stream.
ringBufferE :: Int -> Events a -> Events (Next a)
ringBufferE cap = rwBufferE (newRingIO cap >>= ringRwIO)

-- | Reads to EOF
stdinE :: Events String
stdinE = repeatMayE (catchJust (\e -> if isEOFError e then Just () else Nothing) (fmap Just getLine) (const (pure Nothing)))

stmMapE :: (a -> STM b) -> Events a -> Events b
stmMapE f e = Events (\ctl cb -> produceE e ctl (\ctl' a -> f a >>= cb ctl'))

rawIoMapE :: IO (Rw a b) -> (b -> IO c) -> Events a -> Events c
rawIoMapE mkRw f e = Events $ \ctl cb ->
  let newCb d b = f b >>= atomically . cb d
  in  internalRwRunE mkRw ctl newCb e

ioMapE :: Int -> (Next a -> IO c) -> Events a -> Events c
ioMapE = rawIoMapE . newRingRw

-- | Prints events with timestamps
debugPrintE :: Show a => Events a -> IO ()
debugPrintE e = flip (rwRunE (fmap chanRw newTChanIO)) e $ \_ a -> do
  m <- currentMonoTime
  print (m, a)
