module Tricksy.Internal where

import Control.Applicative (liftA2)
import Control.Concurrent.Async (wait)
import Control.Concurrent.STM (STM, atomically, modifyTVar', newEmptyTMVarIO, retry)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, tryReadTChan, writeTChan)
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
import Tricksy.Active (Active (..), ActiveVar, deactivateVar, newActiveVarIO, readActiveVar, readActiveVarIO, trackActive)
import Tricksy.ActiveScope (allocateActiveVar, scopedActive)
import Tricksy.Cache (CacheHandler)
import Tricksy.Monad (ResM, runResM, spawnAsync, spawnThread, stopThread)
import Tricksy.Time (MonoTime (..), TimeDelta (..), addMonoTime, currentMonoTime, threadDelayDelta)

-- | Event producer - takes a consumer callback and pushes events through.
-- When the consumer callback signals not active, the producer should stop pushing.
newtype Events a = Events {produceE :: (a -> STM Active) -> ResM ()}

instance Functor Events where
  fmap f e = Events (\cb -> produceE e (cb . f))

rawGuardedCall :: STM Active -> STM () -> (a -> STM Active) -> a -> STM Active
rawGuardedCall readActive deactivate cb a = do
  active <- readActive
  case active of
    ActiveNo -> pure ActiveNo
    ActiveYes -> do
      stillActive <- cb a
      case stillActive of
        ActiveNo -> deactivate
        ActiveYes -> pure ()
      pure stillActive

-- | Wrap a callback to check producer liveness before invoking callback and
-- to update the var after invoking it.
guardedCall :: ActiveVar -> (a -> STM Active) -> a -> STM Active
guardedCall activeVar = rawGuardedCall (readActiveVar activeVar) (deactivateVar activeVar)

rawGuardedProduce :: STM Active -> STM () -> Events a -> (a -> STM Active) -> ResM ()
rawGuardedProduce readActive deactivate e cb = produceE e (rawGuardedCall readActive deactivate cb)

-- | Consume while checking producer liveness.
guardedProduce :: ActiveVar -> Events a -> (a -> STM Active) -> ResM ()
guardedProduce activeVar = rawGuardedProduce (readActiveVar activeVar) (deactivateVar activeVar)

instance Applicative Events where
  pure a = Events (\cb -> liftIO (atomically (void (cb a))))
  (<*>) = ap

emptyE :: Events x
emptyE = Events (const (pure ()))

interleaveE :: Events x -> Events x -> Events x
interleaveE el er = Events $ \cb -> scopedActive $ \activeVar -> do
  void (spawnThread (runResM (guardedProduce activeVar el cb)))
  void (spawnThread (runResM (guardedProduce activeVar er cb)))

parallelE :: Foldable f => f (Events x) -> Events x
parallelE es = Events $ \cb -> scopedActive $ \activeVar -> do
  for_ es (\e -> spawnThread (runResM (guardedProduce activeVar e cb)))

andThenE :: Events x -> Events x -> Events x
andThenE e1 e2 = Events $ \cb -> do
  activeVar <- allocateActiveVar
  produceE e1 (guardedCall activeVar cb)
  stillActive <- liftIO (readActiveVarIO activeVar)
  case stillActive of
    ActiveNo -> pure ()
    ActiveYes -> produceE e2 cb

sequentialE :: Foldable f => f (Events x) -> Events x
sequentialE es = res
 where
  res = Events $ \cb -> do
    activeVar <- allocateActiveVar
    let cbg = guardedCall activeVar cb
    case toList es of
      [] -> pure ()
      z : zs -> go activeVar z zs cbg
  go activeVar z zs cbg = do
    produceE z cbg
    stillActive <- liftIO (readActiveVarIO activeVar)
    case stillActive of
      ActiveNo -> pure ()
      ActiveYes ->
        case zs of
          [] -> pure ()
          z' : zs' -> go activeVar z' zs' cbg

spawnChild :: ActiveVar -> ActiveVar -> TMVar a -> TVar Int -> (a -> Events b) -> (b -> STM Active) -> IO ()
spawnChild activeVar parentActiveVar sourceVar genVar f cb = runResM (go Nothing)
 where
  go mc = do
    mz <- liftIO $ atomically $ do
      active <- readActiveVar activeVar
      case active of
        ActiveNo -> pure Nothing
        ActiveYes -> do
          ma <- tryTakeTMVar sourceVar
          case ma of
            Nothing -> do
              parentActive <- readActiveVar parentActiveVar
              case parentActive of
                ActiveNo -> pure Nothing
                ActiveYes -> retry
            Just a -> do
              g <- readTVar genVar
              pure (Just (a, g))
    case mz of
      Nothing -> pure ()
      Just (a, g) -> do
        case mc of
          Just cid -> liftIO (stopThread cid)
          Nothing -> pure ()
        c <- spawnThread (childProduce activeVar genVar g (f a) cb)
        go (Just c)

childProduce :: ActiveVar -> TVar Int -> Int -> Events b -> (b -> STM Active) -> IO ()
childProduce activeVar genVar myGen e cb =
  let readIsMyGen = fmap (== myGen) (readTVar genVar)
      readActive = do
        isMyGen <- readIsMyGen
        if isMyGen then readActiveVar activeVar else pure ActiveNo
      deactivate = deactivateVar activeVar
  in  runResM (rawGuardedProduce readActive deactivate e cb)

parentProduce :: ActiveVar -> ActiveVar -> TMVar a -> TVar Int -> Events a -> IO ()
parentProduce activeVar parentActiveVar sourceVar genVar ea = finally prod clean
 where
  prod = runResM $ guardedProduce activeVar ea $ \a -> do
    putTMVar sourceVar a
    modifyTVar' genVar succ
    pure ActiveYes
  clean = atomically (deactivateVar parentActiveVar)

instance Monad Events where
  return = pure
  ea >>= f = Events $ \cb -> do
    parentActiveVar <- liftIO newActiveVarIO
    sourceVar <- liftIO newEmptyTMVarIO
    genVar <- liftIO (newTVarIO 0)
    scopedActive $ \activeVar -> do
      void (spawnThread (parentProduce activeVar parentActiveVar sourceVar genVar ea))
      spawnAsy <- spawnAsync (spawnChild activeVar parentActiveVar sourceVar genVar f cb)
      -- Need to explicitly wait for the spawner thread to stop so there isn't a race
      -- in checking the thread count ref for automatic termination
      liftIO (wait spawnAsy)

liftE :: IO a -> Events a
liftE act = Events (\cb -> liftIO (act >>= atomically . void . cb))

repeatE :: IO a -> Events a
repeatE act = Events (liftIO . go)
 where
  go cb = do
    a <- act
    active <- atomically (cb a)
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go cb

eachE :: Foldable f => f a -> Events a
eachE fa = Events (liftIO . go (toList fa))
 where
  go as cb =
    case as of
      [] -> pure ()
      a : as' -> do
        active <- atomically (cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go as' cb

capSnoc :: Int -> TVar (Seq a) -> a -> STM ()
capSnoc cap v a =
  modifyTVar' v $ \case
    Empty -> Empty :|> a
    s@(_ :<| t) -> (if Seq.length s >= cap then t else s) :|> a

-- | Fulfills the role of fix
bufferE :: Int -> (STM (Seq a) -> Events a) -> Events a
bufferE cap f = Events $ \cb -> do
  aVar <- liftIO (newTVarIO Seq.empty)
  produceE (f (readTVar aVar)) (\a -> capSnoc cap aVar a *> cb a)

callZipWith :: (a -> b -> c) -> TVar (Maybe a) -> TVar (Maybe b) -> (c -> STM Active) -> a -> STM Active
callZipWith f aVar bVar cb a = do
  void (writeTVar aVar (Just a))
  mb <- readTVar bVar
  case mb of
    Nothing -> pure ActiveYes
    Just b -> cb (f a b)

zipWithE :: (a -> b -> c) -> Events a -> Events b -> Events c
zipWithE f ea eb = Events $ \cb -> do
  aVar <- liftIO (newTVarIO Nothing)
  bVar <- liftIO (newTVarIO Nothing)
  let cba = callZipWith f aVar bVar cb
      cbb = callZipWith (flip f) bVar aVar cb
  scopedActive $ \activeVar -> do
    void (spawnThread (runResM (guardedProduce activeVar ea cba)))
    void (spawnThread (runResM (guardedProduce activeVar eb cbb)))

zipE :: Events a -> Events b -> Events (a, b)
zipE = zipWithE (,)

unfoldE :: (s -> Maybe (a, s)) -> s -> Events a
unfoldE f s0 = Events (\cb -> liftIO (newTVarIO s0 >>= go cb))
 where
  go cb sVar = do
    active <- atomically $ do
      ma <- stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f s))
      maybe (pure ActiveNo) cb ma
    case active of
      ActiveNo -> pure ()
      ActiveYes -> go cb sVar

mapMayE :: (a -> Maybe b) -> Events a -> Events b
mapMayE f e = Events (\cb -> produceE e (maybe (pure ActiveYes) cb . f))

scanE :: (a -> b -> b) -> b -> Events a -> Events b
scanE f b0 e = Events $ \cb -> do
  bVar <- liftIO (newTVarIO b0)
  produceE e (\a -> stateTVar bVar (\b -> let b' = f a b in (b', b')) >>= cb)

scanMayE :: (a -> b -> Maybe b) -> b -> Events a -> Events b
scanMayE f b0 e = Events $ \cb -> do
  bVar <- liftIO (newTVarIO b0)
  produceE e $ \a -> do
    mb <- stateTVar bVar (\b -> maybe (Nothing, b) (\b' -> (Just b', b')) (f a b))
    maybe (pure ActiveYes) cb mb

accumE :: (a -> s -> (b, s)) -> s -> Events a -> Events b
accumE f s0 e = Events $ \cb -> do
  sVar <- liftIO (newTVarIO s0)
  produceE e (\a -> stateTVar sVar (f a) >>= cb)

accumMayE :: (a -> s -> (Maybe b, s)) -> s -> Events a -> Events b
accumMayE f s0 e = Events $ \cb -> do
  sVar <- liftIO (newTVarIO s0)
  produceE e $ \a -> do
    mb <- stateTVar sVar (f a)
    maybe (pure ActiveYes) cb mb

filterE :: (a -> Bool) -> Events a -> Events a
filterE f e = Events (\cb -> produceE e (\a -> if f a then cb a else pure ActiveYes))

filterJustE :: Events (Maybe a) -> Events a
filterJustE e = Events (produceE e . maybe (pure ActiveYes))

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
  nVar <- liftIO (newTVarIO n0)
  produceE e $ \a -> do
    taking <- stateTVar nVar (\n -> if n > 0 then (True, n - 1) else (False, n))
    if taking then cb a else pure ActiveNo

dropE :: Int -> Events a -> Events a
dropE = accumMayE (\a n -> if n > 0 then (Nothing, n - 1) else (Just a, n))

takeWhileE :: (a -> Bool) -> Events a -> Events a
takeWhileE f e = Events (\cb -> produceE e (\a -> if f a then cb a else pure ActiveNo))

dropWhileE :: (a -> Bool) -> Events a -> Events a
dropWhileE f = accumMayE (\a dropping -> if dropping && f a then (Nothing, dropping) else (Just a, False)) True

cycleE :: Foldable f => f a -> Events a
cycleE fa = Events (\cb -> let as0 = toList fa in case as0 of [] -> pure (); _ -> liftIO (go as0 as0 cb))
 where
  go as0 as cb =
    case as of
      [] -> go as0 as0 cb
      a : as' -> do
        active <- atomically (cb a)
        case active of
          ActiveNo -> pure ()
          ActiveYes -> go as0 as' cb

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
  { crActiveVar :: !ActiveVar
  , crAction :: !(STM a)
  }

data HoldRef a = HoldRef
  { hrActiveVar :: !ActiveVar
  , hrCurVar :: !(TVar a)
  }

data Ref a
  = RefPure !a
  | RefCache !(CacheRef a)
  | RefHold !(HoldRef a)

applyWithB :: (a -> b -> c) -> Behavior a -> Events b -> Events c
applyWithB f b e = Events $ \cb -> do
  scopedActive $ \activeVar -> do
    r <- observeB b
    guardedProduce activeVar e (\x -> readR r >>= cb . flip f x)

applyB :: Behavior (a -> b) -> Events a -> Events b
applyB = applyWithB ($)

observeB :: Behavior a -> ResM (Ref a)
observeB = res
 where
  res = \case
    BehaviorPure a -> pure (RefPure a)
    BehaviorCache _ -> error "TODO"
    BehaviorHold (HoldBehavior start e) -> do
      activeVar <- allocateActiveVar
      curVar <- liftIO (newTVarIO start)
      void $
        spawnThread $
          trackActive activeVar (runResM (guardedProduce activeVar e (\a -> ActiveYes <$ writeTVar curVar a)))
      pure (RefHold (HoldRef activeVar curVar))
    BehaviorMerge {} -> error "TODO"

readR :: Ref a -> STM a
readR = \case
  RefPure a -> pure a
  RefCache _ -> error "TODO"
  RefHold (HoldRef _ curVar) -> readTVar curVar

-- | Delays the event stream by some 'TimeDelta'.
-- The delay will happen on the consuming thread.
delayE :: TimeDelta -> Events a -> Events a
delayE delta e = Events (\cb -> liftIO (threadDelayDelta delta) *> produceE e cb)

-- | Try to emit (time, actual delta) with period delta
periodicE :: TimeDelta -> Events (MonoTime, TimeDelta)
periodicE delta = Events (\cb -> liftIO (currentMonoTime >>= go cb))
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

consume :: ActiveVar -> TChan a -> (a -> IO Active) -> IO ()
consume activeVar c f = go
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
          ActiveNo -> atomically (deactivateVar activeVar)
          ActiveYes -> go

-- | Runs the callback on all events in the stream.
runE :: (a -> IO Active) -> Events a -> ResM ()
runE f e = do
  c <- liftIO newTChanIO
  let sourceCb a = ActiveYes <$ writeTChan c a
  scopedActive $ \activeVar -> do
    void (spawnThread (trackActive activeVar (runResM (guardedProduce activeVar e sourceCb))))
    asy <- spawnAsync (consume activeVar c f)
    liftIO (wait asy)

-- | Creates an even stream from an IO action. Returning 'Nothing' ends the stream.
repeatMayE :: IO (Maybe a) -> Events a
repeatMayE f = Events (liftIO . go)
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
channelE activeVar chanVar = Events (liftIO . go)
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
debugPrintE e = runResM $ flip runE e $ \a -> do
  m <- currentMonoTime
  print (m, a)
  pure ActiveYes
