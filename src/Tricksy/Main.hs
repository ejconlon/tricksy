module Tricksy.Main
  ( main
  )
where

import Control.Applicative (Alternative (..))
import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.Async (concurrently_, mapConcurrently_, pollSTM, uninterruptibleCancel, withAsync)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, retry)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (catchJust, finally)
import Control.Monad (void, when)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import System.IO.Error (isEOFError)
import Tricksy.Time (MonoTime, TimeDelta)

main :: IO ()
main = putStrLn "Hello, world!"

-- | Signal from consumers to producers
data Alive = AliveYes | AliveNo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Event producer - takes a consumer callback and pushes events through.
-- When the consumer callback signals not alive, the producer should stop pushing.
newtype Events a = Events {consume :: (a -> IO Alive) -> IO ()}

instance Functor Events where
  fmap f e = Events (\cb -> consume e (cb . f))

-- | Wrap a callback to check producer liveness before invoking callback and
-- to update the var after invoking it.
guardedCall :: TVar Alive -> (a -> IO Alive) -> a -> IO Alive
guardedCall aliveVar cb a = do
  alive <- readTVarIO aliveVar
  case alive of
    AliveNo -> pure AliveNo
    AliveYes -> do
      stillAlive <- cb a
      case stillAlive of
        AliveNo -> atomically (writeTVar aliveVar AliveNo)
        AliveYes -> pure ()
      pure stillAlive

-- | Consume while checking producer liveness.
guardedConsume :: TVar Alive -> Events a -> (a -> IO Alive) -> IO ()
guardedConsume aliveVar e cb = consume e (guardedCall aliveVar cb)

-- | Consume async with thread killed when producer dies.
guardedAsyncConsume :: TVar Alive -> Events a -> (a -> IO Alive) -> IO ()
guardedAsyncConsume aliveVar e cb = do
  withAsync (guardedConsume aliveVar e cb) $ \async -> do
    shouldCancel <- atomically $ do
      mres <- pollSTM async
      case mres of
        Nothing -> do
          alive <- readTVar aliveVar
          case alive of
            AliveYes -> retry
            AliveNo -> pure True
        Just res -> do
          case res of
            Left _ -> writeTVar aliveVar AliveNo
            Right _ -> pure ()
          pure False
    when shouldCancel (uninterruptibleCancel async)

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

callAp :: TVar (Ap a b) -> (TVar (Ap a b) -> x -> STM (Maybe b)) -> (b -> IO Alive) -> x -> IO Alive
callAp apVar write cb x = do
  mb <- atomically (write apVar x)
  case mb of
    Nothing -> pure AliveYes
    Just b -> cb b

instance Applicative Events where
  pure a = Events (\cb -> void (cb a))
  el <*> er = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    apVar <- newTVarIO (Ap Nothing Nothing)
    let cbl = callAp apVar writeApLeft cb
        cbr = callAp apVar writeApRight cb
    concurrently_ (guardedAsyncConsume aliveVar el cbl) (guardedAsyncConsume aliveVar er cbr)

instance Alternative Events where
  empty = Events (const (pure ()))
  el <|> er = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    concurrently_ (guardedAsyncConsume aliveVar el cb) (guardedAsyncConsume aliveVar er cb)

parallel :: Foldable f => f (Events x) -> Events x
parallel es = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  mapConcurrently_ (\e -> guardedAsyncConsume aliveVar e cb) es

andThen :: Events x -> Events x -> Events x
andThen e1 e2 = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  consume e1 (guardedCall aliveVar cb)
  stillAlive <- readTVarIO aliveVar
  case stillAlive of
    AliveNo -> pure ()
    AliveYes -> consume e2 cb

sequential :: Foldable f => f (Events x) -> Events x
sequential es = res
 where
  res = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    let cbg = guardedCall aliveVar cb
    case toList es of
      [] -> pure ()
      z : zs -> go aliveVar z zs cbg
  go aliveVar z zs cbg = do
    consume z cbg
    stillAlive <- readTVarIO aliveVar
    case stillAlive of
      AliveNo -> pure ()
      AliveYes ->
        case zs of
          [] -> pure ()
          z' : zs' -> go aliveVar z' zs' cbg

instance Monad Events where
  return = pure
  _ea >>= _f = undefined

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

callZipWith :: (a -> b -> c) -> TMVar a -> TMVar b -> (c -> IO Alive) -> a -> IO Alive
callZipWith f aVar bVar cb a = do
  mc <- atomically $ do
    putTMVar aVar a
    mb <- tryTakeTMVar bVar
    case mb of
      Nothing ->
        pure Nothing
      Just b -> do
        _ <- takeTMVar aVar
        pure (Just (f a b))
  case mc of
    Nothing -> pure AliveYes
    Just c -> cb c

zipWithE :: (a -> b -> c) -> Events a -> Events b -> Events c
zipWithE f ea eb = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  aVar <- newEmptyTMVarIO
  bVar <- newEmptyTMVarIO
  let cba = callZipWith f aVar bVar cb
      cbb = callZipWith (flip f) bVar aVar cb
  concurrently_ (guardedAsyncConsume aliveVar ea cba) (guardedAsyncConsume aliveVar eb cbb)

zipE :: Events a -> Events b -> Events (a, b)
zipE = zipWithE (,)

scanE :: (a -> b -> b) -> b -> Events a -> Events b
scanE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consume e (\a -> atomically (stateTVar bVar (\b -> let b' = f a b in (b', b'))) >>= cb)

withScanE :: (a -> b -> b) -> b -> Events a -> Events (b, a)
withScanE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consume e (\a -> atomically (stateTVar bVar (\b -> let b' = f a b in (b', b'))) >>= \b' -> cb (b', a))

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

withCountE :: Events a -> Events (Int, a)
withCountE = withScanE (const (+ 1)) 0

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
