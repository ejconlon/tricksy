module Tricksy.Main
  ( main
  )
where
import Data.Sequence (Seq)
import Control.Concurrent.Async (mapConcurrently_, concurrently_)
import Data.Map.Strict (Map)
import Control.Concurrent.STM.TVar (TVar, writeTVar, newTVarIO, readTVar, stateTVar, readTVarIO)
import Control.Concurrent (ThreadId, killThread, forkFinally)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Monad (when, void, ap)
import Control.Applicative (Alternative (..))
import Control.Exception (finally, catchJust)
import Tricksy.Time (TimeDelta, MonoTime, currentMonoTime)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import System.IO.Error (isEOFError)
import Data.Bifunctor (first)

main :: IO ()
main = putStrLn "Hello, world!"

data Alive = AliveYes | AliveNo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype Stream a = Stream { unStream :: (a -> IO Alive) -> IO () }

instance Functor Stream where
  fmap f (Stream x) = Stream (\cb -> x (cb . f))

data Ap a b = Ap
  { apLeft :: !(Maybe (a -> b))
  , apRight :: !(Maybe a)
  }

writeApLeft :: TVar (Ap a b) -> (a -> b) -> STM (Maybe b)
writeApLeft apVar f = stateTVar apVar $ \(Ap _ ma) ->
  let ap' = Ap (Just f) ma
  in case ma of
    Nothing -> (Nothing, ap')
    Just a -> (Just (f a), ap')

writeApRight :: TVar (Ap a b) -> a -> STM (Maybe b)
writeApRight apVar a = stateTVar apVar $ \(Ap mf _) ->
  let ap' = Ap mf (Just a)
  in case mf of
    Nothing -> (Nothing, ap')
    Just f -> (Just (f a), ap')

data ApState b = ApStateContinue | ApStateEmit b | ApStateHalt

callAp :: TVar Alive -> TVar (Ap a b) -> (b -> IO Alive) -> (TVar (Ap a b) -> x -> STM (Maybe b)) -> x -> IO Alive
callAp aliveVar apVar cb write x = do
  aps <- atomically $ do
    alive <- readTVar aliveVar
    case alive of
      AliveNo -> pure ApStateHalt
      AliveYes -> fmap (maybe ApStateContinue ApStateEmit) (write apVar x)
  case aps of
    ApStateContinue -> pure AliveYes
    ApStateEmit b -> do
      stillAlive <- cb b
      case stillAlive of
        AliveNo -> atomically (writeTVar aliveVar stillAlive)
        AliveYes -> pure ()
      pure stillAlive
    ApStateHalt -> pure AliveNo

instance Applicative Stream where
  pure a = Stream (\cb -> void (cb a))
  Stream xf <*> Stream xa = Stream $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    apVar <- newTVarIO (Ap Nothing Nothing)
    let cbl = callAp aliveVar apVar cb writeApLeft
        cbr = callAp aliveVar apVar cb writeApRight
    concurrently_ (xf cbl) (xa cbr)

callAlt :: TVar Alive -> (b -> IO Alive) -> b -> IO Alive
callAlt aliveVar cb b = do
  alive <- readTVarIO aliveVar
  case alive of
    AliveNo -> pure alive
    AliveYes -> do
      stillAlive <- cb b
      case stillAlive of
        AliveNo -> atomically (writeTVar aliveVar stillAlive)
        AliveYes -> pure ()
      pure stillAlive

instance Alternative Stream where
  empty = Stream (const (pure ()))
  Stream x1 <|> Stream x2 = Stream $ \cb -> do
    aliveVar <- newTVarIO AliveYes
    let cb' = callAlt aliveVar cb
    concurrently_ (x1 cb') (x2 cb')

parallel :: Foldable f => f (Stream x) -> Stream x
parallel ss = Stream $ \cb -> do
  aliveVar <- newTVarIO AliveYes
  let cb' = callAlt aliveVar cb
  mapConcurrently_ (\(Stream x) -> x cb') ss

instance Monad Stream where
  return = pure
  Stream xa >>= f = Stream $ \cb -> do
    undefined

once :: IO a -> Stream a
once act = Stream (\cb -> void (act >>= cb))

repeatedly :: IO a -> Stream a
repeatedly act = Stream go where
  go cb = do
    a <- act
    alive <- cb a
    case alive of
      AliveNo -> pure ()
      AliveYes -> go cb

scan :: (a -> b -> b) -> b -> Stream a -> Stream b
scan f b0 (Stream xa) = Stream $ \cb -> do
  bVar <- newTVarIO b0
  xa (\a -> atomically (stateTVar bVar (\b -> let b' = f a b in (b', b'))) >>= cb)

scanMay :: (a -> b -> Maybe b) -> b -> Stream a -> Stream b
scanMay f b0 (Stream xa) = Stream $ \cb -> do
  bVar <- newTVarIO b0
  xa $ \a -> do
    mb <- atomically (stateTVar bVar (\b -> maybe (Nothing, b) (\b' -> (Just b', b')) (f a b)))
    maybe (pure AliveYes) cb mb

mapMay :: (a -> Maybe b) -> Stream a -> Stream b
mapMay f (Stream xa) = Stream (\cb -> xa (maybe (pure AliveYes) cb . f))

accum :: (a -> s -> (b, s)) -> s -> Stream a -> Stream b
accum f s0 (Stream xa) = Stream $ \cb -> do
  sVar <- newTVarIO s0
  xa (\a -> atomically (stateTVar sVar (f a)) >>= cb)

accumMay ::  (a -> s -> Maybe (b, s)) -> s -> Stream a -> Stream b
accumMay f s0 (Stream xa) = Stream $ \cb -> do
  sVar <- newTVarIO s0
  xa $ \a -> do
    mb <- atomically (stateTVar sVar (\s -> maybe (Nothing, s) (first Just) (f a s)))
    maybe (pure AliveYes) cb mb

filters ::  (a -> Bool) -> Stream a -> Stream a
filters f (Stream xa) = Stream (\cb -> xa (\a -> if f a then cb a else pure AliveYes))

filterJust ::  Stream (Maybe a) -> Stream a
filterJust (Stream xma) = Stream (xma . maybe (pure AliveYes))

splits ::  Stream (Either a b) -> (Stream a, Stream b)
splits s = (lefts s, rights s)

lefts ::  Stream (Either a b) -> Stream a
lefts (Stream xeab) = Stream (\cb -> xeab (either cb (const (pure AliveYes))))

rights ::  Stream (Either a b) -> Stream b
rights (Stream xeab) = Stream (xeab . either (const (pure AliveYes)))

iterates :: (a -> a) -> a -> Stream b -> Stream a
iterates = undefined

withIterates :: (a -> a) -> a -> Stream b -> Stream (a, b)
withIterates = undefined

sums :: Num a => Stream a -> Stream a
sums = scan (+) 0

products :: Num a => Stream a -> Stream a
products = scan (*) 1

count :: Stream a -> Stream Int
count = scan (const (+1)) 0

withCount :: Stream a -> Stream (Int, a)
withCount = undefined

appends :: Monoid a => Stream a -> Stream a
appends = scan (flip (<>)) mempty

foldMaps :: Monoid b => (a -> b) -> Stream a -> Stream b
foldMaps f = scan (flip (<>) . f) mempty

takes :: Int -> Stream a -> Stream a
takes = undefined

drops :: Int -> Stream a -> Stream a
drops = undefined

takesWhile :: (a -> Bool) -> Stream a -> Stream a
takesWhile = undefined

dropsWhile :: (a -> Bool) -> Stream a -> Stream a
dropsWhile = undefined

cycles :: [a] -> Stream b -> Stream a
cycles = undefined

seqAt :: Seq a -> Stream Int -> Stream a
seqAt = undefined

mapAt :: Ord k => Map k v -> Stream k -> Stream v
mapAt = undefined

data Hold a = Hold
  { holdStart :: !a
  , holdStream :: !(Stream a)
  , holdRelease :: !(Maybe (IO ()))
  } deriving stock (Functor)

data Signal a =
    SignalPure !a
  | SignalHold !(Hold a)
  deriving stock (Functor)

instance Applicative Signal where
  pure = SignalPure
  (<*>) = undefined

hold :: a -> Stream a -> Maybe (IO ()) -> Signal a
hold start stream mrelease = SignalHold (Hold start stream mrelease)

unHold :: Signal a -> Stream a
unHold = \case
  SignalPure a -> pure a
  SignalHold (Hold start (Stream xa) mrelease) -> Stream $ \cb ->
    let go = cb start >>= \case { AliveNo -> pure (); AliveYes -> xa cb }
    in maybe go (finally go) mrelease

data HoldRef a = HoldRef
  { hrAliveVar :: !(TVar Alive)
  , hrCurVar :: !(TVar a)
  , hrTid :: !ThreadId
  }

data Ref a =
    RefPure !a
  | RefHold !(HoldRef a)

guardedWrite :: TVar Alive -> TVar a -> a -> IO Alive
guardedWrite aliveVar curVar val = atomically $ do
  alive <- readTVar aliveVar
  case alive of
    AliveYes -> writeTVar curVar val
    AliveNo -> pure ()
  pure alive

guardedRelease :: TVar Alive -> Maybe (IO ()) -> IO ()
guardedRelease aliveVar mrelease =
  case mrelease of
    Nothing -> atomically (writeTVar aliveVar AliveNo)
    Just release -> do
      cleanup <- atomically $ stateTVar aliveVar $ \alive ->
        case alive of
          AliveYes -> (True, AliveNo)
          AliveNo -> (False, alive)
      when cleanup release

ref :: Signal a -> IO (Ref a)
ref = \case
  SignalPure a -> pure (RefPure a)
  SignalHold (Hold start (Stream ex) mrelease) -> do
    aliveVar <- newTVarIO AliveYes
    curVar <- newTVarIO start
    tid <- forkFinally (ex (guardedWrite aliveVar curVar)) (const (guardedRelease aliveVar mrelease))
    pure (RefHold (HoldRef aliveVar curVar tid))

observe :: Ref a -> STM a
observe = \case
  RefPure a -> pure a
  RefHold (HoldRef _ curVar _) -> readTVar curVar

dispose :: Ref a -> IO ()
dispose = \case
  RefPure _ -> pure ()
  RefHold (HoldRef _ _ tid) -> killThread tid

clock :: TimeDelta -> Stream MonoTime
clock = undefined

pulse :: TimeDelta -> Stream ()
pulse = undefined

ticks :: TimeDelta -> Stream TimeDelta
ticks = undefined

timer :: TimeDelta -> Stream TimeDelta
timer = undefined

periodic :: TimeDelta -> IO Alive -> IO ()
periodic delta act = undefined

channel :: TVar Alive -> TChan a -> Stream a
channel aliveVar chanVar = Stream go where
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
stdin :: Stream String
stdin = Stream go where
  go cb = do
    ms <- catchJust (\e -> if isEOFError e then Just () else Nothing) (fmap Just getLine) (const (pure Nothing))
    case ms of
      Nothing -> pure ()
      Just s -> do
        alive <- cb s
        case alive of
          AliveNo -> pure ()
          AliveYes -> go cb
