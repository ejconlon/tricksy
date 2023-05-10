-- TODO frame zippers for dom-style capturing/bubbling?
-- TODO focus tracking
-- TODO explicit exports
-- TODO widgets
-- TODO ui events
module Tricksy.Vty where

import Control.Applicative (Alternative (..), liftA2)
import Control.Exception (bracket)
import Control.Monad (void, when, (<=<))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, runReader)
import Control.Monad.State.Strict (MonadState (..), State, StateT (..), gets, modify', runState)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (foldl', toList)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Graphics.Vty qualified as V
import Lens.Micro (Lens', lens)

class Monoid n => Group n where
  invert :: n -> n
  sub :: n -> n -> n
  sub x y = x <> invert y

instance Num n => Group (Sum n) where
  invert (Sum n) = Sum (negate n)

class (Num n, Ord n) => NumHalvable n where
  halveNum :: n -> (n, n)

instance NumHalvable Int where
  halveNum i = let x = div i 2 in (x, i - x)

instance NumHalvable Double where
  halveNum i = let x = i / 2 in (x, i - x)

class (Monoid n, Ord n) => Halvable n where
  halve :: n -> (n, n)

instance NumHalvable n => Halvable (Sum n) where
  halve (Sum n) = let (x, y) = halveNum n in (Sum x, Sum y)

type SizeLike n = (Halvable n, Group n, Ord n)

data V2 n = V2 {v2X :: !n, v2Y :: !n}
  deriving stock (Eq, Ord, Show)

instance Semigroup n => Semigroup (V2 n) where
  V2 x1 y1 <> V2 x2 y2 = V2 (x1 <> x2) (y1 <> y2)

instance Monoid n => Monoid (V2 n) where
  mempty = V2 mempty mempty
  mappend = (<>)

instance Group n => Group (V2 n) where
  invert (V2 x y) = V2 (invert x) (invert y)

instance Halvable n => Halvable (V2 n) where
  halve (V2 x y) =
    let (x1, x2) = halve x
        (y1, y2) = halve y
    in  (V2 x1 y1, V2 x2 y2)

data Layout = LayoutHoriz | LayoutVert
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Align = AlignBegin | AlignMiddle | AlignEnd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data BoxAlign = BoxAlign {baHoriz :: !Align, baVert :: !Align}
  deriving stock (Eq, Ord, Show)

defBoxAlign :: BoxAlign
defBoxAlign = BoxAlign AlignBegin AlignBegin

data Box n = Box
  { boxAlign :: !BoxAlign
  , boxTopLeftOff :: !(V2 n)
  , boxContentSize :: !(V2 n)
  , boxBotRightOff :: !(V2 n)
  }
  deriving stock (Eq, Ord, Show)

box :: Monoid n => V2 n -> Box n
box sz = Box defBoxAlign mempty sz mempty

boxSize :: Semigroup n => Box n -> V2 n
boxSize (Box _ (V2 tlx tly) (V2 cx cy) (V2 brx bry)) =
  V2 (tlx <> cx <> brx) (tly <> cy <> bry)

data Elem b r z
  = ElemPart !z
  | ElemLayout !Layout !b !r !(Seq r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (Elem b) where
  bimap f g = \case
    ElemPart z -> ElemPart (g z)
    ElemLayout ly bo r rs -> ElemLayout ly bo (f r) (fmap f rs)

instance Bifoldable (Elem b) where
  bifoldr f g x = \case
    ElemPart z -> g z x
    ElemLayout _ _ r rs -> f r (foldr f x rs)

instance Bitraversable (Elem b) where
  bitraverse f g = \case
    ElemPart z -> fmap ElemPart (g z)
    ElemLayout ly bo r rs -> liftA2 (ElemLayout ly bo) (f r) (traverse f rs)

data FrameF n r = FrameF {frameBoxF :: !(Box n), frameContentF :: !r}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Frame n b z = Frame {unFrame :: FrameF n (Elem b (Frame n b z) z)}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

frame :: (SizeLike n, HasSize n z, HasFill n b) => Elem b (Frame n b z) z -> Frame n b z
frame = \case
  ElemPart z -> framePart z
  ElemLayout ly bo f fs -> cat ly bo f fs

frameBox :: Frame n b z -> Box n
frameBox = frameBoxF . unFrame

setFrameBox :: Frame n b z -> Box n -> Frame n b z
setFrameBox (Frame (FrameF _ c)) b = Frame (FrameF b c)

frameBoxL :: Lens' (Frame n b z) (Box n)
frameBoxL = lens frameBox setFrameBox

frameElem :: Frame n b z -> Elem b (Frame n b z) z
frameElem = frameContentF . unFrame

setFrameElem :: Frame n b z -> Elem b (Frame n b z) z -> Frame n b z
setFrameElem (Frame (FrameF b _)) c = Frame (FrameF b c)

frameElemL :: Lens' (Frame n b z) (Elem b (Frame n b z) z)
frameElemL = lens frameElem setFrameElem

frameCata :: (FrameF n (Elem b x z) -> x) -> Frame n b z -> x
frameCata f = go where go = f . fmap (first go) . unFrame

frameCataM :: Monad m => (FrameF n (Elem b x z) -> m x) -> Frame n b z -> m x
frameCataM f = go where go = f <=< traverse (bitraverse go pure) . unFrame

framePart :: (Monoid n, HasSize n z) => z -> Frame n b z
framePart z = Frame (FrameF (box (getSize z)) (ElemPart z))

frameMap :: (SizeLike n, HasSize n w, HasFill n b) => (z -> w) -> Frame n b z -> Frame n b w
frameMap f = frameCata (frame . fmap f . frameContentF)

frameBind :: (SizeLike n, HasSize n w, HasFill n b) => (z -> Frame n b w) -> Frame n b z -> Frame n b w
frameBind f = frameCata (elemBind f . frameContentF)

elemBind :: (SizeLike n, HasSize n w, HasFill n b) => (z -> Frame n b w) -> Elem b (Frame n b w) z -> Frame n b w
elemBind f = \case
  ElemPart z -> f z
  ElemLayout ly bo r rs -> cat ly bo r rs

newtype ImBorder = ImBorder {unImBorder :: Bool}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

class HasFill n b where
  getFill :: b -> n

instance HasFill (Sum Int) ImBorder where
  getFill (ImBorder b) = if b then Sum 1 else mempty

instance Monoid n => HasFill n () where
  getFill _ = mempty

class HasSize n a where
  getWidth :: a -> n
  getWidth = v2X . getSize
  getHeight :: a -> n
  getHeight = v2Y . getSize
  getSize :: a -> V2 n
  getSize a = V2 (getWidth a) (getHeight a)
  {-# MINIMAL (getWidth, getHeight) | getSize #-}

instance HasSize (Sum Int) V.Image where
  getWidth = Sum . V.imageWidth
  getHeight = Sum . V.imageHeight

instance HasSize n (V2 n) where
  getWidth = v2X
  getHeight = v2Y
  getSize = id

instance Semigroup n => HasSize n (Box n) where
  getWidth (Box _ (V2 a _) (V2 b _) (V2 c _)) = a <> b <> c
  getHeight (Box _ (V2 _ a) (V2 _ b) (V2 _ c)) = a <> b <> c

instance (Monoid n, Ord n, HasSize n r, HasSize n z, HasFill n b) => HasSize n (Elem b r z) where
  getSize = \case
    ElemPart z -> getSize z
    ElemLayout l b a as -> cat l b (getSize a) (fmap getSize as)

instance (Semigroup n, HasSize n a) => HasSize n (Frame n b a) where
  getSize = getSize . frameBoxF . unFrame

instance Monoid n => HasSize n () where
  getWidth _ = mempty
  getHeight _ = mempty
  getSize _ = mempty

class Widenable a where
  widen :: Layout -> a -> a -> (a, a)

instance Ord n => Widenable (V2 n) where
  widen ly (V2 x1 y1) (V2 x2 y2) =
    case ly of
      LayoutHoriz -> let y = max y1 y2 in (V2 x1 y, V2 x2 y)
      LayoutVert -> let x = max x1 x2 in (V2 x y1, V2 x y2)

fitBox :: Halvable n => V2 n -> Box n -> Box n
fitBox (V2 dx dy) (Box ba@(BoxAlign bax bay) (V2 tlx tly) sz (V2 brx bry)) =
  let (tlx', brx') = case bax of
        AlignBegin -> (tlx, brx <> dx)
        AlignMiddle -> let (tld, brd) = halve dx in (tlx <> tld, brx <> brd)
        AlignEnd -> (tlx <> dx, brx)
      (tly', bry') = case bay of
        AlignBegin -> (tly, bry <> dy)
        AlignMiddle -> let (tld, brd) = halve dy in (tly <> tld, bry <> brd)
        AlignEnd -> (tly <> dy, bry)
  in  Box ba (V2 tlx' tly') sz (V2 brx' bry')

instance SizeLike n => Widenable (Box n) where
  widen ly b1 b2 =
    let V2 x1 y1 = getSize b1
        V2 x2 y2 = getSize b2
        (d1, d2) = case ly of
          LayoutHoriz ->
            let y = max y1 y2
                e1 = V2 mempty (sub y y1)
                e2 = V2 mempty (sub y y2)
            in  (e1, e2)
          LayoutVert ->
            let x = max x1 x2
                e1 = V2 (sub x x1) mempty
                e2 = V2 (sub x x2) mempty
            in  (e1, e2)
        b1' = fitBox d1 b1
        b2' = fitBox d2 b2
    in  (b1', b2')

instance SizeLike n => Widenable (Frame n b z) where
  widen ly f1 f2 =
    let b1 = frameBox f1
        b2 = frameBox f2
        (b1', b2') = widen ly b1 b2
    in  (setFrameBox f1 b1', setFrameBox f2 b2')

class Stackable b a where
  hstack :: b -> a -> a -> a
  hstack = stack LayoutHoriz
  vstack :: b -> a -> a -> a
  vstack = stack LayoutVert
  stack :: Layout -> b -> a -> a -> a
  stack = \case LayoutHoriz -> hstack; LayoutVert -> vstack
  hcat :: b -> a -> Seq a -> a
  hcat = foldl' . hstack
  vcat :: b -> a -> Seq a -> a
  vcat = foldl' . vstack
  cat :: Layout -> b -> a -> Seq a -> a
  cat = \case LayoutHoriz -> hcat; LayoutVert -> vcat
  {-# MINIMAL stack | (hstack, vstack) #-}

instance (Monoid n, Ord n, HasFill n b) => Stackable b (V2 n) where
  stack ly bo (V2 x1 y1) (V2 x2 y2) =
    let z = getFill bo
    in  case ly of
          LayoutHoriz -> V2 (x1 <> z <> x2) (max y1 y2)
          LayoutVert -> V2 (max x1 x2) (y1 <> z <> y2)

instance Stackable V.Image V.Image where
  hstack bo im1 im2 = V.horizCat [im1, bo, im2]
  vstack bo im1 im2 = V.vertCat [im1, bo, im2]
  hcat bo hd tl = V.horizCat (intersperse bo (hd : toList tl))
  vcat bo hd tl = V.vertCat (intersperse bo (hd : toList tl))

instance (Monoid n, Ord n, HasFill n b) => Stackable b (Box n) where
  stack ly bo b1 b2 = Box defBoxAlign mempty (stack ly bo (getSize b1) (getSize b2)) mempty

-- TODO impl cat to reduce tree depth
instance (SizeLike n, HasSize n z, HasFill n b) => Stackable b (Frame n b z) where
  stack ly bo (Frame (FrameF b1 c1)) (Frame (FrameF b2 c2)) =
    let (b1', b2') = widen ly b1 b2
        f1' = Frame (FrameF b1' c1)
        f2' = Frame (FrameF b2' c2)
        b = stack ly bo b1' b2'
        c = ElemLayout ly bo f1' (Seq.singleton f2')
    in  Frame (FrameF b c)

build :: (Semigroup n, Stackable c w) => (n -> Layout -> b -> c) -> (Box n -> z -> w) -> Frame n b z -> w
build g f = frameCata $ \(FrameF bx e) ->
  case e of
    ElemPart z -> f bx z
    ElemLayout ly bo w ws ->
      let n = case ly of LayoutHoriz -> getHeight bx; LayoutVert -> getWidth bx
      in  cat ly (g n ly bo) w ws

buildM :: (Monad m, Semigroup n, Stackable c w) => (n -> Layout -> b -> m c) -> (Box n -> z -> m w) -> Frame n b z -> m w
buildM g f = frameCataM $ \(FrameF bx e) ->
  case e of
    ElemPart z -> f bx z
    ElemLayout ly bo w ws ->
      let n = case ly of LayoutHoriz -> getHeight bx; LayoutVert -> getWidth bx
      in  fmap (\c -> cat ly c w ws) (g n ly bo)

hitBox :: (Semigroup n, Ord n) => V2 n -> Box n -> Bool
hitBox (V2 hr hc) (Box _ (V2 tlr tlc) (V2 nr nc) _) = hr >= tlr && hr < tlr <> nr && hc >= tlc && hc < tlc <> nc

layoutOffset :: (Group n, HasFill n b) => Box n -> Layout -> b -> V2 n
layoutOffset bx ly bo =
  let z = getFill bo
  in  case ly of
        LayoutHoriz -> V2 (invert (getWidth bx <> z)) mempty
        LayoutVert -> V2 mempty (invert (getHeight bx <> z))

-- | Find the innermost hit
hitFrame :: (Group n, Ord n, HasFill n b) => V2 n -> Frame n b z -> Maybe z
hitFrame initPos fr = goRoot
 where
  goRoot = flip runReader initPos $ flip frameCata fr $ \(FrameF bx con) ->
    case con of
      ElemPart z -> asks (\pos -> if hitBox pos bx then Just z else Nothing)
      ElemLayout ly bo r rs -> goFold (layoutOffset bx ly bo) mempty (r : toList rs)
  goFold layOff !off = \case
    [] -> pure Nothing
    r : rs -> do
      mz <- local (off <>) r
      case mz of
        Just _ -> pure mz
        Nothing -> goFold layOff (off <> layOff) rs

type ImFrame = Frame (Sum Int) ImBorder V.Image

buildI :: V.Attr -> ImFrame -> V.Image
buildI at = build @(Sum Int) @V.Image @V.Image onBorder onContent
 where
  onBorder (Sum n) ly bo =
    let (Sum z) = getFill bo
        (c, w, h) = case ly of LayoutHoriz -> ('|', z, n); LayoutVert -> ('-', n, z)
    in  V.charFill at c w h
  onContent (Box _ (V2 (Sum tlx) (Sum tly)) (V2 (Sum cx) (Sum cy)) (V2 (Sum brx) (Sum bry))) im =
    V.pad tlx tly brx bry (V.crop cx cy im)

showI :: V.Image -> IO ()
showI im = do
  let pic = V.picForImage im
  cfg <- V.standardIOConfig
  bracket (V.mkVty cfg) V.shutdown $ \vty -> do
    V.update vty pic
    void (V.nextEvent vty)

testI :: V.Attr -> ImFrame
testI at =
  let bo = ImBorder True
  in  hstack
        bo
        (vstack bo (framePart (V.string at "hello")) (framePart (V.string at "world")))
        (framePart (V.string at ":)"))

data Event n
  = EvKey !V.Key !(Set V.Modifier)
  | EvMouseDown !(V2 n) !V.Button !(Set V.Modifier)
  | EvMouseUp !(V2 n)
  | EvResize !(V2 n)
  | EvPaste !Text
  | EvLostFocus
  | EvGainedFocus
  deriving stock (Eq, Ord, Show)

convertEv :: V.Event -> Event (Sum Int)
convertEv = \case
  V.EvKey k ms -> EvKey k (Set.fromList ms)
  V.EvMouseDown r c b ms -> EvMouseDown (V2 (Sum r) (Sum c)) b (Set.fromList ms)
  V.EvMouseUp r c _ -> EvMouseUp (V2 (Sum r) (Sum c))
  V.EvResize r c -> EvResize (V2 (Sum r) (Sum c))
  V.EvPaste bs -> EvPaste (TE.decodeUtf8 bs)
  V.EvLostFocus -> EvLostFocus
  V.EvGainedFocus -> EvGainedFocus

-- newtype W a = W { unW ::

data Widget m e n v = Widget
  { widgetBox :: !(Box n)
  , widgetCallback :: !(V2 n -> e -> m ())
  , widgetView :: !(m v)
  }

instance Monoid n => HasSize n (Widget m e n v) where
  getWidth = getWidth . widgetBox
  getHeight = getHeight . widgetBox
  getSize = getSize . widgetBox

widgetPure :: (Applicative m, Monoid n, HasSize n v) => v -> Widget m e n v
widgetPure v = Widget (box (getSize v)) (\_ _ -> pure ()) (pure v)

instance (Applicative m, SizeLike n, HasSize n v, HasFill n b, Stackable b v) => Stackable b (Widget m e n v) where
  stack ly bo (Widget bx1 cb1 v1) (Widget bx2 cb2 v2) = Widget bx cb v
   where
    (bx1', bx2') = widen ly bx1 bx2
    bx = stack ly bo bx1' bx2'
    diff =
      let z = getFill bo
      in  case ly of
            LayoutHoriz -> V2 (invert (getWidth bx1' <> z)) mempty
            LayoutVert -> V2 mempty (invert (getHeight bx1' <> z))
    cb p e =
      if hitBox p bx1'
        then cb1 p e
        else let p' = p <> diff in when (hitBox p bx2') (cb2 p' e)
    v = liftA2 (stack ly bo) v1 v2

-- buildW :: (Applicative m, HasSize n v) => Frame n (Widget m e n v) -> Widget m e n v
-- buildW = build onBorder onContent where

newtype Id = Id {unId :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

data Ref n = Ref {refId :: !Id, refOffset :: !(V2 n)}
  deriving stock (Eq, Ord, Show)

type IdFrame n b z = FrameF n (Elem b (Ref n) z)

data Focus n = Focus
  { focId :: !Id
  , focCursor :: !(V2 n)
  }
  deriving stock (Eq, Ord, Show)

data Camera n = Camera
  { camRoot :: !Id
  , camFocus :: !(Maybe (Focus n))
  }
  deriving stock (Eq, Ord, Show)

data St n b z = St
  { stNextId :: !Id
  , stFrames :: !(Map Id (IdFrame n b z))
  -- , stCamera :: !(Maybe (Camera n))
  }
  deriving stock (Eq, Ord, Show)

newtype M n b z a = M {unM :: State (St n b z) a}
  deriving newtype (Functor, Applicative, Monad, MonadState (St n b z))

runM :: M n b z a -> St n b z -> (a, St n b z)
runM = runState . unM

addFrameF :: IdFrame n b z -> M n b z Id
addFrameF r =
  state (\(St k m) -> (k, St {stNextId = succ k, stFrames = Map.insert k r m}))

foldState :: (s -> a -> (b, s)) -> s -> Seq a -> (Seq b, s)
foldState f = go Empty
 where
  go !acc !s = \case
    Empty -> (acc, s)
    a :<| as -> let (b, s') = f s a in go (acc :|> b) s' as

addOffset :: (Group n, HasFill n b) => FrameF n (Elem b Id z) -> IdFrame n b z
addOffset (FrameF bx c) = FrameF bx (goElem c)
 where
  goElem = \case
    ElemPart z -> ElemPart z
    ElemLayout ly bo r rs ->
      ElemLayout ly bo (Ref r mempty) (fst (foldState (goFold (layoutOffset bx ly bo)) mempty rs))
  goFold layOff off i = let off' = off <> layOff in (Ref i off', off')

addFrame :: (Group n, HasFill n b) => Frame n b z -> M n b z Id
addFrame = frameCataM (addFrameF . addOffset)

lookupFrame :: Id -> M n b z (Maybe (IdFrame n b z))
lookupFrame i = gets (Map.lookup i . stFrames)

newtype X m a = X {unX :: StateT (Set Id) (MaybeT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadState (Set Id), Alternative)

instance MonadTrans X where
  lift = X . lift . lift

runX :: X m a -> Set Id -> m (Maybe (a, Set Id))
runX x s = runMaybeT (runStateT (unX x) s)

evalX :: Functor m => X m a -> m (Maybe a)
evalX x = fmap (fmap fst) (runX x Set.empty)

extractFrame :: Id -> M n b z (Maybe (Frame n b z))
extractFrame = fmap (fmap Frame) . evalX . go
 where
  go i = do
    mjr <- lift (lookupFrame i)
    case mjr of
      Nothing -> empty
      Just r -> do
        seen <- get
        if Set.member i seen
          then empty
          else do
            modify' (Set.insert i)
            traverse (bitraverse (fmap Frame . go . refId) pure) r

-- hitFrameM :: (Semigroup n, Ord n) => V2 n -> M n b z (Maybe (Id, IdFrame n b z))
-- hitFrameM pos = runMaybeT goRoot where
--   goRoot = do
--     q <- MaybeT (gets (fmap camRoot . stCamera))
--     goSearch Nothing (Seq.singleton q)
--   goSearch best = \case
--     Empty -> maybe empty pure best
--     q :<| rest -> do
--       r@(FrameF bx c) <- MaybeT (lookupFrame q)
--       let p = (q, r)
--       if hitBox pos bx
--         then case c of
--           ElemPart _ -> pure p
--           ElemLayout _ _ s ss -> goSearch (Just p) (s :<| (ss <> rest))
--         else goSearch (Just p) rest

-- data Dir = DirLeft | DirRight | DirUp | DirDown
--   deriving stock (Eq, Ord, Show)

-- keyDir :: V.Key -> Maybe Dir
-- keyDir = \case
--   V.KLeft -> Just DirLeft
--   V.KRight -> Just DirRight
--   V.KUp -> Just DirUp
--   V.KDown -> Just DirDown
--   _ -> Nothing

-- -- dirTargetFrame :: Dir -> M n b z (Maybe (Id, IdFrame n b z))
-- -- dirTargetFrame dir = runMaybeT goRoot where
-- --   goRoot = do
-- --     foc <- MaybeT (gets (camFocus <=< stCamera))
-- --     undefined
--     -- r <- MaybeT (lookupFrame q)
--     -- goSearch Nothing (Seq.singleton (q, r))
