module Tricksy.Internal where

import Control.Applicative (liftA2)
import Control.Concurrent.STM (STM, atomically, modifyTVar', newEmptyTMVarIO, orElse, retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Exception (catchJust, finally)
import Control.Monad (ap, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Foldable (for_, toList)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import System.IO.Error (isEOFError)
import Tricksy.Active (Active (..))
import Tricksy.Cache (CacheHandler)
import Tricksy.Control (Control (..), allocateControl, scopedControl, trackControl)
import Tricksy.Monad (ResM, runResM, spawnThread, stopThread)
import Tricksy.Time (MonoTime (..), TimeDelta (..), addMonoTime, currentMonoTime, threadDelayDelta)

guarded :: Control -> STM () -> STM Active
guarded ctl act = do
  active <- controlReadActive ctl
  case active of
    ActiveNo -> pure ActiveNo
    ActiveYes -> act *> controlReadActive ctl

guarded_ :: Control -> STM () -> STM ()
guarded_ ctl act = do
  active <- controlReadActive ctl
  case active of
    ActiveNo -> pure ()
    ActiveYes -> act

guardedIO_ :: Control -> IO () -> IO ()
guardedIO_ ctl act = do
  active <- atomically (controlReadActive ctl)
  case active of
    ActiveNo -> pure ()
    ActiveYes -> act

-- | Consumer callback
type Callback m a = Control -> a -> m ()

guardedCall :: Control -> Callback STM a -> a -> STM Active
guardedCall ctl cb a = guarded ctl (cb ctl a)

guardedCall_ :: Control -> Callback STM a -> a -> STM ()
guardedCall_ ctl cb a = guarded_ ctl (cb ctl a)

-- | Event producer - takes a consumer callback and pushes events through.
newtype Events a = Events {produceE :: Control -> Callback STM a -> ResM ()}

instance Functor Events where
  fmap f e = Events (\ctl cb -> produceE e ctl (\d a -> cb d (f a)))

instance Applicative Events where
  pure a = Events (\ctl cb -> liftIO (atomically (guardedCall_ ctl cb a)))
  (<*>) = ap

emptyE :: Events x
emptyE = Events (\_ _ -> pure ())

interleaveE :: Events x -> Events x -> Events x
interleaveE el er = Events $ \ctl cb -> scopedControl ctl $ do
  void (spawnThread (runResM (produceE el ctl cb)))
  void (spawnThread (runResM (produceE er ctl cb)))

parallelE :: Foldable f => f (Events x) -> Events x
parallelE es = Events $ \ctl cb -> scopedControl ctl $ do
  for_ es (\e -> spawnThread (runResM (produceE e ctl cb)))

andThenE :: Events x -> Events x -> Events x
andThenE e1 e2 = Events $ \ctl cb -> do
  produceE e1 ctl cb
  active <- liftIO (atomically (controlReadActive ctl))
  case active of
    ActiveNo -> pure ()
    ActiveYes -> produceE e2 ctl cb

sequentialE :: [Events x] -> Events x
sequentialE es = Events $ \c cb -> do
  case es of
    [] -> pure ()
    z : zs -> produceE (z `andThenE` sequentialE zs) c cb

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
      active <- controlReadActive conCtl
      case active of
        ActiveNo -> pure Nothing
        ActiveYes -> do
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
        newCid <- spawnThread (runResM (produceE e' ctl' cb))
        case mc of
          Just oldCid -> liftIO (stopThread oldCid)
          Nothing -> pure ()
        go (Just newCid)

parentProduce :: Control -> Control -> TMVar a -> TVar Int -> Events a -> IO ()
parentProduce prodCtl conCtl sourceVar genVar ea = finally prod clean
 where
  prod = runResM $ produceE ea conCtl $ \_ a -> do
    putTMVar sourceVar a
    modifyTVar' genVar succ
  clean = atomically (controlDeactivate prodCtl)

instance Monad Events where
  return = pure
  ea >>= f = Events $ \conCtl cb -> do
    prodCtl <- allocateControl
    sourceVar <- liftIO newEmptyTMVarIO
    genVar <- liftIO (newTVarIO 0)
    scopedControl conCtl $ do
      void (spawnThread (parentProduce prodCtl conCtl sourceVar genVar ea))
      void (spawnThread (spawnChild prodCtl conCtl sourceVar genVar f cb))

liftE :: IO a -> Events a
liftE act = Events (\ctl cb -> liftIO (guardedIO_ ctl (act >>= atomically . guardedCall_ ctl cb)))

repeatE :: IO a -> Events a
repeatE act = Events (\ctl cb -> liftIO (guardedIO_ ctl (go ctl cb)))
 where
  go ctl cb = do
    a <- liftIO act
    active <- liftIO (atomically (guardedCall ctl cb a))
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

-- | Fulfills the role of fix
bufferE :: Int -> (STM (Seq a) -> Events a) -> Events a
bufferE cap f = Events $ \ctl cb -> do
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
    void (spawnThread (runResM (produceE ea ctl cba)))
    void (spawnThread (runResM (produceE eb ctl cbb)))

-- TODO work out termination

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

data CacheBehaviorSpec a where
  CacheBehaviorSpec :: !TimeDelta -> !(CacheHandler z a) -> !(IO z) -> CacheBehaviorSpec a

instance Functor CacheBehaviorSpec where
  fmap f (CacheBehaviorSpec ttl han act) = CacheBehaviorSpec ttl (fmap f . han) act

newtype CacheBehavior a = CacheBehavior {unCacheBehavior :: ResM (CacheBehaviorSpec a)}

instance Functor CacheBehavior where
  fmap f = CacheBehavior . fmap (fmap f) . unCacheBehavior

data HoldBehavior a = HoldBehavior
  { hbStart :: !a
  , hbEvents :: !(Events a)
  }
  deriving stock (Functor)

applyWithH :: (a -> b -> c) -> HoldBehavior a -> HoldBehavior b -> HoldBehavior c
applyWithH f (HoldBehavior sa ea) (HoldBehavior sb eb) = HoldBehavior (f sa sb) (zipWithE f ea eb)

applyH :: HoldBehavior (a -> b) -> HoldBehavior a -> HoldBehavior b
applyH = applyWithH ($)

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
cacheB ttl han act = BehaviorCache (CacheBehavior (pure (CacheBehaviorSpec ttl han act)))

explicitCacheB :: CacheBehavior a -> Behavior a
explicitCacheB = BehaviorCache

holdB :: a -> Events a -> Behavior a
holdB start e = BehaviorHold (HoldBehavior start e)

explicitHoldB :: HoldBehavior a -> Behavior a
explicitHoldB = BehaviorHold

data CacheRef a = CacheRef
  { crControl :: !Control
  , crAction :: !(STM a)
  }

data HoldRef a = HoldRef
  { hrControl :: !Control
  , hrCurVar :: !(TVar a)
  }

data Ref a
  = RefPure !a
  | RefCache !(CacheRef a)
  | RefHold !(HoldRef a)

applyWithB :: (a -> b -> c) -> Behavior a -> Events b -> Events c
applyWithB f b e = Events $ \ctl cb -> do
  scopedControl ctl $ do
    r <- observeB b
    produceE e ctl (\d x -> readR r >>= cb d . flip f x)

applyB :: Behavior (a -> b) -> Events a -> Events b
applyB = applyWithB ($)

observeB :: Behavior a -> ResM (Ref a)
observeB = res
 where
  res = \case
    BehaviorPure a -> pure (RefPure a)
    BehaviorCache _ -> error "TODO"
    BehaviorHold (HoldBehavior start e) -> do
      ctl <- allocateControl
      curVar <- liftIO (newTVarIO start)
      void $
        spawnThread $
          runResM (produceE e ctl (\_ a -> writeTVar curVar a))
      pure (RefHold (HoldRef ctl curVar))
    BehaviorMerge {} -> error "TODO"

readR :: Ref a -> STM a
readR = \case
  RefPure a -> pure a
  RefCache _ -> error "TODO"
  RefHold (HoldRef _ curVar) -> readTVar curVar

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

consumeIO :: Control -> Control -> TChan a -> Callback IO a -> IO ()
consumeIO prodCtl conCtl chan f = go
 where
  go = do
    ma <- atomically $ do
      -- First check if consumer is done
      conActive <- controlReadActive conCtl
      case conActive of
        ActiveNo -> pure Nothing
        ActiveYes -> do
          -- Try to read next item from chan
          ma <- tryReadTChan chan
          case ma of
            Just _ -> pure ma
            Nothing -> do
              -- If nothing, wait only if prod is active
              prodActive <- controlReadActive prodCtl
              case prodActive of
                ActiveNo -> pure Nothing
                ActiveYes -> retry
    case ma of
      Nothing -> pure ()
      Just a -> f conCtl a *> go

-- | Runs the callback on all events in the stream.
runE :: Callback IO a -> Events a -> ResM ()
runE f e = do
  chan <- liftIO newTChanIO
  let sourceCb = const (writeTChan chan)
  prodCtl <- allocateControl
  conCtl <- allocateControl
  scopedControl conCtl $ do
    void (spawnThread (trackControl prodCtl (runResM (produceE e conCtl sourceCb))))
    void (spawnThread (consumeIO prodCtl conCtl chan f))

-- | Creates an even stream from an IO action. Returning 'Nothing' ends the stream.
repeatMayE :: IO (Maybe a) -> Events a
repeatMayE f = Events (\ctl cb -> liftIO (guardedIO_ ctl (go ctl cb)))
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

repeatTxnE :: STM (Maybe a) -> Events a
repeatTxnE act = Events (\ctl cb -> liftIO (go ctl cb))
 where
  go ctl cb = do
    active <- atomically (guarded ctl (act >>= maybe (pure ()) (cb ctl)))
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go ctl cb

retryControl :: Control -> STM (Maybe a) -> STM (Maybe a)
retryControl ctl act = do
  ma <- act
  case ma of
    Nothing -> do
      active <- controlReadActive ctl
      case active of
        ActiveNo -> pure ma
        ActiveYes -> retry
    Just _ -> pure ma

-- | Events from a channel
channelE :: Control -> TChan a -> Events a
channelE prodCtl chanVar = repeatTxnE (retryControl prodCtl (tryReadTChan chanVar))

-- | Events from a lock var
lockE :: Control -> TMVar a -> Events a
lockE prodCtl lockVar = repeatTxnE (retryControl prodCtl (tryTakeTMVar lockVar))

-- | Reads to EOF
stdinE :: Events String
stdinE = repeatMayE (catchJust (\e -> if isEOFError e then Just () else Nothing) (fmap Just getLine) (const (pure Nothing)))

-- | Prints events with timestamps
debugPrintE :: Show a => Events a -> IO ()
debugPrintE e = runResM $ flip runE e $ \_ a -> do
  m <- currentMonoTime
  print (m, a)
