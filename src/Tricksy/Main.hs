module Tricksy.Main
  ( main
  , Events
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
import Control.Concurrent.Async (Async, cancel, concurrently_, mapConcurrently_, pollSTM, withAsync)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, retry)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (catchJust, finally, mask, throwIO)
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
newtype Events a = Events {consumeE :: (a -> IO Alive) -> IO ()}

instance Functor Events where
  fmap f e = Events (\cb -> consumeE e (cb . f))

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
guardedConsume aliveVar e cb = consumeE e (guardedCall aliveVar cb)

-- | Consume async with thread killed when producer dies.
guardedAsyncConsume :: TVar Alive -> Events a -> (a -> IO Alive) -> IO ()
guardedAsyncConsume aliveVar e cb = do
  withAsync (guardedConsume aliveVar e cb) $ \async -> do
    (shouldCancel, merr) <- atomically $ do
      mres <- pollSTM async
      case mres of
        Nothing -> do
          alive <- readTVar aliveVar
          case alive of
            AliveYes -> retry
            AliveNo -> pure (True, Nothing)
        Just res -> do
          case res of
            Left err -> do
              writeTVar aliveVar AliveNo
              pure (False, Just err)
            Right _ -> pure (False, Nothing)
    let cleanup = when shouldCancel (cancel async)
    maybe cleanup (finally cleanup . throwIO) merr

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

parallelE :: Foldable f => f (Events x) -> Events x
parallelE es = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  mapConcurrently_ (\e -> guardedAsyncConsume aliveVar e cb) es

andThenE :: Events x -> Events x -> Events x
andThenE e1 e2 = Events $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  consumeE e1 (guardedCall aliveVar cb)
  stillAlive <- readTVarIO aliveVar
  case stillAlive of
    AliveNo -> pure ()
    AliveYes -> consumeE e2 cb

sequentialE :: Foldable f => f (Events x) -> Events x
sequentialE es = res
 where
  res = Events $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    let cbg = guardedCall aliveVar cb
    case toList es of
      [] -> pure ()
      z : zs -> go aliveVar z zs cbg
  go aliveVar z zs cbg = do
    consumeE z cbg
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

liftE :: IO a -> Events a
liftE act = Events (\cb -> void (act >>= cb))

repeatE :: IO a -> Events a
repeatE act = Events go
 where
  go cb = do
    a <- act
    alive <- cb a
    case alive of
      AliveNo -> pure ()
      AliveYes -> go cb

eachE :: Foldable f => f a -> Events a
eachE fa = Events (go (toList fa))
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

unfoldE :: (s -> Maybe (a, s)) -> s -> Events a
unfoldE f s0 = Events (\cb -> newTVarIO s0 >>= go cb)
 where
  go cb sVar = do
    ma <- atomically (stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f s)))
    case ma of
      Nothing -> pure ()
      Just a -> do
        alive <- cb a
        case alive of
          AliveNo -> pure ()
          AliveYes -> go cb sVar

mapMayE :: (a -> Maybe b) -> Events a -> Events b
mapMayE f e = Events (\cb -> consumeE e (maybe (pure AliveYes) cb . f))

scanE :: (a -> b -> b) -> b -> Events a -> Events b
scanE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consumeE e (\a -> atomically (stateTVar bVar (\b -> let b' = f a b in (b', b'))) >>= cb)

scanMayE :: (a -> b -> Maybe b) -> b -> Events a -> Events b
scanMayE f b0 e = Events $ \cb -> do
  bVar <- newTVarIO b0
  consumeE e $ \a -> do
    mb <- atomically (stateTVar bVar (\b -> maybe (Nothing, b) (\b' -> (Just b', b')) (f a b)))
    maybe (pure AliveYes) cb mb

accumE :: (a -> s -> (b, s)) -> s -> Events a -> Events b
accumE f s0 e = Events $ \cb -> do
  sVar <- newTVarIO s0
  consumeE e (\a -> atomically (stateTVar sVar (f a)) >>= cb)

accumMayE :: (a -> s -> Maybe (b, s)) -> s -> Events a -> Events b
accumMayE f s0 e = Events $ \cb -> do
  sVar <- newTVarIO s0
  consumeE e $ \a -> do
    mb <- atomically (stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f a s)))
    maybe (pure AliveYes) cb mb

filterE :: (a -> Bool) -> Events a -> Events a
filterE f e = Events (\cb -> consumeE e (\a -> if f a then cb a else pure AliveYes))

filterJustE :: Events (Maybe a) -> Events a
filterJustE e = Events (consumeE e . maybe (pure AliveYes))

leftE :: Events (Either a b) -> Events a
leftE e = Events (\cb -> consumeE e (either cb (const (pure AliveYes))))

rightE :: Events (Either a b) -> Events b
rightE e = Events (consumeE e . either (const (pure AliveYes)))

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

enumerateE :: Events a -> Events (Int, a)
enumerateE = accumE (\a i -> ((i, a), i + 1)) 0

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

consumeHold :: a -> Events a -> (a -> IO Alive) -> IO ()
consumeHold start e cb = do
  alive <- cb start
  case alive of
    AliveNo -> pure ()
    AliveYes -> consumeE e cb

edgeE :: Behavior a -> Events a
edgeE = \case
  BehaviorPure a -> pure a
  BehaviorHold mh -> Events $ \cb -> mask $ \restore -> do
    Hold start e mrel <- mh
    let go = restore (consumeHold start e cb)
    maybe go (finally go) mrel

data HoldRef a = HoldRef
  { hrCurVar :: !(TVar a)
  , hrAsync :: !(Async ())
  }

data Ref a
  = RefPure !a
  | RefHold !(HoldRef a)

refB :: Behavior a -> IO (Ref a)
refB = res
 where
  res = \case
    BehaviorPure a -> pure (RefPure a)
    BehaviorHold mh -> mask $ \restore -> do
      Hold start e mrel <- mh
      curVar <- newTVarIO start
      let bgInner = restore (consumeE e (\a -> AliveYes <$ atomically (writeTVar curVar a)))
          bgOuter = maybe bgInner (finally bgInner) mrel
      withAsync bgOuter (pure . RefHold . HoldRef curVar)

observeR :: Ref a -> STM a
observeR = \case
  RefPure a -> pure a
  RefHold (HoldRef curVar _) -> readTVar curVar

disposeR :: Ref a -> IO ()
disposeR = \case
  RefPure _ -> pure ()
  RefHold (HoldRef _ asy) -> cancel asy

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

-- | Events from a (closable) channel
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
