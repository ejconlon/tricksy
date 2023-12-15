-- TODO frame zippers for dom-style capturing/bubbling?
-- TODO focus tracking
-- TODO explicit exports
-- TODO widgets
-- TODO ui events
module Tricksy.Vty where

import Control.Applicative (Alternative (..), liftA2)
import Control.Exception (bracket)
import Control.Monad (void, when, (<=<))
import Control.Monad.Reader (MonadReader (..), asks, runReader)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
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
import Lens.Micro (Lens', lens, set)
import Lens.Micro.Extras (view)

-- Various lens helpers

using :: MonadState s m => Lens' s a -> m a
using l = gets (view l)

usingAs :: MonadState s m => Lens' s a -> (a -> b) -> m b
usingAs l f = gets (f . view l)

modifying :: MonadState s m => Lens' s a -> (a -> a) -> m ()
modifying l f = modify' (\s -> let a' = f (view l s) in set l a' s)

stating :: MonadState s m => Lens' s a -> (a -> (b, a)) -> m b
stating l f = state (\s -> let (b, a') = f (view l s) in (b, set l a' s))

-- We need to settle on numeric types to represent points - integral for
-- CLI use; decimal for other purposes. We can get by with anything we
-- can add, subtract, and halve.

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

instance NumHalvable Integer where
  halveNum i = let x = div i 2 in (x, i - x)

instance NumHalvable Double where
  halveNum i = let x = i / 2 in (x, i - x)

instance NumHalvable Rational where
  halveNum i = let x = i / 2 in (x, i - x)

class (Monoid n, Ord n) => Halvable n where
  halve :: n -> (n, n)

instance NumHalvable n => Halvable (Sum n) where
  halve (Sum n) = let (x, y) = halveNum n in (Sum x, Sum y)

type SizeLike n = (Halvable n, Group n, Ord n)

-- A 2D vector
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

-- Our layout is based on n-ary trees of boxes grouped horizontally
-- or vertically for each layer with justified contents.

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
  -- ^ Alignment of children
  , boxTopLeftOff :: !(V2 n)
  -- ^ Offset from top left corner of self to same of content
  , boxContentSize :: !(V2 n)
  -- ^ Size of contents
  , boxBotRightOff :: !(V2 n)
  -- ^ Offset from bottom right corner of content to same of self
  }
  deriving stock (Eq, Ord, Show)

box :: Monoid n => V2 n -> Box n
box sz = Box defBoxAlign mempty sz mempty

-- A box's size includes padding, so it's no smaller than its content's size
boxSize :: Semigroup n => Box n -> V2 n
boxSize (Box _ (V2 tlx tly) (V2 cx cy) (V2 brx bry)) =
  V2 (tlx <> cx <> brx) (tly <> cy <> bry)

-- Content of a single level of our layout tree. Either an element of our
-- domain or a layout
-- [@b@] The type of border
-- [@z@] The type of element
-- [@r@] The recursive type (Frame, which contains more elems)
data Elem b z r
  = ElemPart !z
  | ElemLayout !Layout !b !r !(Seq r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (Elem b) where
  bimap f g = \case
    ElemPart z -> ElemPart (f z)
    ElemLayout ly bo r rs -> ElemLayout ly bo (g r) (fmap g rs)

instance Bifoldable (Elem b) where
  bifoldr f g x = \case
    ElemPart z -> f z x
    ElemLayout _ _ r rs -> g r (foldr g x rs)

instance Bitraversable (Elem b) where
  bitraverse f g = \case
    ElemPart z -> fmap ElemPart (f z)
    ElemLayout ly bo r rs -> liftA2 (ElemLayout ly bo) (g r) (traverse g rs)

-- A single level of our layout tree. Pairs contents with a bounding box.
-- Invariant: The bounding box contains the boxes of its children.
-- [@n@] The type numeric type for boxes
-- [@r@] The recursive type (Frame)
data FrameF n b z r = FrameF {frameBoxF :: !(Box n), frameContentF :: !(Elem b z r) }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (FrameF n b) where
  bimap f g = undefined

instance Bifoldable (FrameF n b) where
  bifoldr f g x = undefined

instance Bitraversable (FrameF n b) where
  bitraverse f g = undefined

-- The knot tying our layout tree together.
newtype Frame n b z = Frame {unFrame :: FrameF n b z (Frame n b z)}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- Smart constructor for frames from elements
frame :: (SizeLike n, HasSize n z, HasFill n b) => Elem b z (Frame n b z) -> Frame n b z
frame = \case
  ElemPart z -> framePart z
  ElemLayout ly bo f fs -> cat ly bo f fs

-- Smart constructor for frames from domain types
framePart :: (Monoid n, HasSize n z) => z -> Frame n b z
framePart z = Frame (FrameF (box (getSize z)) (ElemPart z))

-- frameLayout ::

-- getters/setters/lenses

frameBox :: Frame n b z -> Box n
frameBox = frameBoxF . unFrame

setFrameBox :: Frame n b z -> Box n -> Frame n b z
setFrameBox (Frame (FrameF _ c)) b = Frame (FrameF b c)

frameBoxL :: Lens' (Frame n b z) (Box n)
frameBoxL = lens frameBox setFrameBox

frameElem :: Frame n b z -> Elem b z (Frame n b z)
frameElem = frameContentF . unFrame

setFrameElem :: Frame n b z -> Elem b z (Frame n b z) -> Frame n b z
setFrameElem (Frame (FrameF b _)) c = Frame (FrameF b c)

frameElemL :: Lens' (Frame n b z) (Elem b z (Frame n b z))
frameElemL = lens frameElem setFrameElem

-- How to fold frames

frameCata :: (FrameF n b z r -> r) -> Frame n b z -> r
frameCata f = go where go = f . fmap go . unFrame

frameCataM :: Monad m => (FrameF n b z r -> m r) -> Frame n b z -> m r
frameCataM f = go where go = f <=< traverse go . unFrame

-- Map and bind for frames - note that we have to constrain the types we can map to those
-- we can size, so we can't implement Functor/Monad

frameMap :: (SizeLike n, HasSize n w, HasFill n b) => (z -> w) -> Frame n b z -> Frame n b w
frameMap f = frameCata (frame . first f . frameContentF)

frameMapM :: (Monad m, SizeLike n, HasSize n w, HasFill n b) => (z -> m w) -> Frame n b z -> m (Frame n b w)
frameMapM f = frameCataM (fmap frame . bitraverse f pure . frameContentF)

frameBind :: (SizeLike n, HasSize n w, HasFill n b) => (z -> Frame n b w) -> Frame n b z -> Frame n b w
frameBind f = frameCata (elemBind f . frameContentF)

-- frameBindM ::

elemBind :: (SizeLike n, HasSize n w, HasFill n b) => (z -> Frame n b w) -> Elem b z (Frame n b w) -> Frame n b w
elemBind f = \case
  ElemPart z -> f z
  ElemLayout ly bo r rs -> cat ly bo r rs

-- Border represented by 1-row/column of symbol in VTY
-- True means visible.
newtype ImBorder = ImBorder {unImBorder :: Bool}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- Border types we can get the width XOR height of
class HasFill n b where
  getFill :: b -> n

instance HasFill (Sum Int) ImBorder where
  getFill (ImBorder b) = if b then Sum 1 else mempty

instance Monoid n => HasFill n () where
  getFill _ = mempty

-- Things we can get the width AND height of
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

-- Allows us to calculate a bounding box that contains both objects
class Widenable a where
  widen :: Layout -> a -> a -> (a, a)

instance Ord n => Widenable (V2 n) where
  widen ly (V2 x1 y1) (V2 x2 y2) =
    case ly of
      LayoutHoriz -> let y = max y1 y2 in (V2 x1 y, V2 x2 y)
      LayoutVert -> let x = max x1 x2 in (V2 x y1, V2 x y2)

-- Fits a box into a larger box with the given padding.
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

-- Allows us to vertically/horizontally stack things @a@ with borders @b@
-- The minimal implementation is @stack@ but you may want to implement
-- @cat@ for more compact trees.
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

data Crumb d a r = Crumb
  { crumbUp :: !r
  , crumbAnno :: !d
  , crumbPrev :: !(Seq a)
  , crumbNext :: !(Seq a)
  } deriving stock (Eq, Ord, Show)

data Zip d a = Zip { zipCrumb :: !(Maybe (Crumb d a (Zip d a))), zipContents :: !a }
  deriving stock (Eq, Ord, Show)

type ZipUp d a = d -> Seq a -> Seq a -> a -> a

type ZipDown d a = a -> Maybe (d, Seq a, a)

type ZipIndex d a = Int -> a -> Maybe (d, Seq a, Seq a, a)

withZip :: ZipUp d b -> (Zip d a -> Zip d b) -> a -> b
withZip f g = zipOut f . g . zipIn

zipUp :: ZipUp d a -> Zip d a -> Maybe (Zip d a)
zipUp f (Zip mc ct) = fmap (\(Crumb (Zip mc' _) d p n) -> Zip mc' (f d p n ct)) mc

zipOut :: ZipUp d a -> Zip d a -> a
zipOut f (Zip mc0 ct0) = go mc0 ct0 where
  go mc ct = case mc of
    Nothing -> ct
    Just (Crumb (Zip mc' _) d p n) -> go mc' (f d p n ct)

zipIn :: a -> Zip d a
zipIn = Zip Nothing

zipIndex :: ZipIndex d a -> Int -> Zip d a -> Maybe (Zip d a)
zipIndex f ix u@(Zip _ ct) = fmap (\(d, p, n, ct') -> Zip (Just (Crumb u d p n)) ct') (f ix ct)

zipDown :: ZipDown d a -> Zip d a -> Maybe (Zip d a)
zipDown f u@(Zip _ ct) = fmap (\(d, n, ct') -> Zip (Just (Crumb u d Empty n)) ct') (f ct)

zipPrev :: Zip d a -> Maybe (Zip d a)
zipPrev (Zip mc ct) = mc >>= \(Crumb u d p n) ->
  case p of
    Empty -> Nothing
    p' :|> a -> Just (Zip (Just (Crumb u d p' (ct :<| n))) a)

zipNext :: Zip d a -> Maybe (Zip d a)
zipNext (Zip mc ct) = mc >>= \(Crumb u d p n) ->
  case n of
    Empty -> Nothing
    a :<| n' -> Just (Zip (Just (Crumb u d (p :|> ct) n')) a)

type FrameZip n b z = Zip Layout (Frame n b z)

fzUpFn :: ZipUp Layout (Frame n b z)
fzUpFn a d p n = undefined

fzIndexFn :: ZipIndex Layout (Frame n b z)
fzIndexFn ix a = undefined

fzDownFn :: ZipDown Layout (Frame n b z)
fzDownFn a = undefined

fzIn :: Frame n b z -> FrameZip n b z
fzIn = zipIn

fzUp :: FrameZip n b z -> Maybe (FrameZip n b z)
fzUp = zipUp fzUpFn

fzOut :: FrameZip n b z -> Frame n b z
fzOut = zipOut fzUpFn

fzIndex :: Int -> FrameZip n b z -> Maybe (FrameZip n b z)
fzIndex = zipIndex fzIndexFn

fzDown :: FrameZip n b z -> Maybe (FrameZip n b z)
fzDown = zipDown fzDownFn

fzMap :: (SizeLike n, HasSize n z, HasFill n b) => (z -> z) -> FrameZip n b z -> FrameZip n b z
fzMap f (Zip mc ct) = Zip mc (frameMap f ct)

-- fzMapM :: (Monad m, SizeLike n, HasSize n z, HasFill n b) => (z -> m z) -> FrameZip n b z -> FrameZip n b z
-- fzMapM f (Zip mc ct) = fmap (Zip mc) (frameMapM f ct)

withFz :: (FrameZip n b z -> FrameZip n b w) -> Frame n b z -> Frame n b w
withFz = withZip fzUpFn

-- data Hit n b z =
--     HitBorder !Layout !b !Int
--   | HitDomain !z
--   | HitFrame !(FrameF n b z (Hit n b z))
--   deriving stock (Eq, Ord, Show)

-- hitFrameF :: V2 n -> FrameF n b z (Maybe (Hit n b z)) -> Maybe (Hit n b z)

type ImFrame = Frame (Sum Int) ImBorder V.Image

buildI :: V.Attr -> ImFrame -> V.Image
buildI boAttr = build @(Sum Int) @V.Image @V.Image onBorder onContent
 where
  onBorder (Sum n) ly bo =
    let (Sum z) = getFill bo
        (c, w, h) = case ly of LayoutHoriz -> ('|', z, n); LayoutVert -> ('-', n, z)
    in  V.charFill boAttr c w h
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
testI txAttr =
  let bo = ImBorder True
  in  hstack
        bo
        (vstack bo (framePart (V.string txAttr "hello")) (framePart (V.string txAttr "world")))
        (framePart (V.string txAttr ":)"))

runTestI :: IO ()
runTestI = let at = V.defAttr in showI (buildI at (testI at))

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

-- TODO implement
-- buildW :: (Applicative m, HasSize n v) => Frame n (Widget m e n v) -> Widget m e n v
-- buildW = build onBorder onContent where

newtype Id = Id {unId :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum)

-- TODO what is offset from here?
data Ref n = Ref {refId :: !Id, refOffset :: !(V2 n)}
  deriving stock (Eq, Ord, Show)

type IdFrame n b z = FrameF n b z (Ref n)

data Focus n = Focus
  { focId :: !Id
  , focCursor :: !(V2 n)
  }
  deriving stock (Eq, Ord, Show)

data CameraSt n = CameraSt
  { csRoot :: !Id
  , csFocus :: !(Maybe (Focus n))
  }
  deriving stock (Eq, Ord, Show)

class HasCameraSt n s | s -> n where
  cameraStL :: Lens' s (CameraSt n)

instance HasCameraSt n (CameraSt n) where
  cameraStL = id

type MonadCameraSt n s m = (MonadState s m, HasCameraSt n s)

data FrameSt n b z = FrameSt
  { fsNextId :: !Id
  , fsFrames :: !(Map Id (IdFrame n b z))
  }
  deriving stock (Eq, Ord, Show)

class HasFrameSt n b z s | s -> n b z where
  frameStL :: Lens' s (FrameSt n b z)

instance HasFrameSt n b z (FrameSt n b z) where
  frameStL = id

type MonadFrameSt n b z s m = (MonadState s m, HasFrameSt n b z s)

addFrameF :: MonadFrameSt n b z s m => IdFrame n b z -> m Id
addFrameF r = stating frameStL $ \(FrameSt k m) ->
  (k, FrameSt (succ k) (Map.insert k r m))

foldState :: (s -> a -> (b, s)) -> s -> Seq a -> (Seq b, s)
foldState f = go Empty
 where
  go !acc !s = \case
    Empty -> (acc, s)
    a :<| as -> let (b, s') = f s a in go (acc :|> b) s' as

addOffset :: (Group n, HasFill n b) => FrameF n b z Id -> IdFrame n b z
addOffset (FrameF bx c) = FrameF bx (goElem c)
 where
  goElem = \case
    ElemPart z -> ElemPart z
    ElemLayout ly bo r rs ->
      ElemLayout ly bo (Ref r mempty) (fst (foldState (goFold (layoutOffset bx ly bo)) mempty rs))
  goFold layOff off i = let off' = off <> layOff in (Ref i off', off')

addFrame :: (Group n, HasFill n b, MonadFrameSt n b z s m) => Frame n b z -> m Id
addFrame = frameCataM (addFrameF . addOffset)

lookupFrame :: MonadFrameSt n b z s m => Id -> m (Maybe (IdFrame n b z))
lookupFrame i = gets (Map.lookup i . fsFrames . view frameStL)

newtype X m a = X {unX :: StateT (Set Id) (MaybeT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadState (Set Id), Alternative)

instance MonadTrans X where
  lift = X . lift . lift

runX :: X m a -> Set Id -> m (Maybe (a, Set Id))
runX x s = runMaybeT (runStateT (unX x) s)

evalX :: Functor m => X m a -> m (Maybe a)
evalX x = fmap (fmap fst) (runX x Set.empty)

-- extractFrame :: MonadFrameSt n b z s m => Id -> m (Maybe (Frame n b z))
-- extractFrame = fmap (fmap Frame) . evalX . go
--  where
--   go i = do
--     mjr <- lift (lookupFrame i)
--     case mjr of
--       Nothing -> empty
--       Just r -> do
--         seen <- get
--         if Set.member i seen
--           then empty
--           else do
--             modify' (Set.insert i)
--             traverse (bitraverse pure (fmap Frame . go . refId)) r

data BorderHit b = BorderHit !Layout !b !Int

type FrameHit n b z = FrameF n (Either (BorderHit b) z)

-- hitFrameM :: (Semigroup n, Ord n, MonadFrameSt n b z s m) => V2 n -> Id -> m (Maybe (Id, FrameHit n b z))
-- hitFrameM pos0 q0 = runMaybeT (go Nothing Empty pos0 q0)
--  where
--   go best rest pos q = do
--     r@(FrameF bx c) <- MaybeT (lookupFrame q)
--     let p = (q, r)
--     if hitBox pos bx
--       then case c of
--         ElemPart _ -> pure p
--         ElemLayout ly bo s ss ->
--           let layOff = layoutOffset bx ly bo
--           in  error "TODO"
--       else case rest of
--         Empty -> pure best
--         (pos', q') :<| rest' -> go best rest' pos' q'

data Dir = DirLeft | DirRight | DirUp | DirDown
  deriving stock (Eq, Ord, Show)

keyDir :: V.Key -> Maybe Dir
keyDir = \case
  V.KLeft -> Just DirLeft
  V.KRight -> Just DirRight
  V.KUp -> Just DirUp
  V.KDown -> Just DirDown
  _ -> Nothing

-- -- dirTargetFrame :: Dir -> M n b z (Maybe (Id, IdFrame n b z))
-- -- dirTargetFrame dir = runMaybeT goRoot where
-- --   goRoot = do
-- --     foc <- MaybeT (gets (camFocus <=< stCamera))
-- --     undefined
--     -- r <- MaybeT (lookupFrame q)
--     -- goSearch Nothing (Seq.singleton (q, r))
