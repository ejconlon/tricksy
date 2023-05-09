-- TODO frame zippers for dom-style capturing/bubbling?
-- TODO focus tracking
-- TODO explicit exports
module Tricksy.Vty where

import Control.Exception (bracket)
import Control.Monad (void, (<=<))
import Control.Monad.Reader (MonadReader (..), asks, runReader)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (foldl', toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Sum (..))
import Graphics.Vty qualified as V
import Lens.Micro (Lens', lens)

class Monoid n => Group n where
  invert :: n -> n
  sub :: n -> n -> n
  sub x y = x <> invert y

instance Num n => Group (Sum n) where
  invert (Sum n) = Sum (negate n)

-- -- | Numbers extended with an "undefined" point
-- newtype Ext n = Ext { unExt :: Maybe n }
--   deriving stock (Show)
--   deriving newtype (Eq, Ord)

-- instance Semigroup n => Semigroup (Ext n) where
--   ea <> eb = case unExt ea of
--     Nothing -> eb
--     Just a -> case unExt eb of
--       Nothing -> ea
--       Just b -> Ext (Just (a <> b))

-- instance Semigroup n => Monoid (Ext n) where
--   mempty = Ext Nothing
--   mappend = (<>)

-- instance Group n => Group (Ext n) where
--   invert (Ext mn) = Ext (fmap invert mn)

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

-- instance Halvable n => Halvable (Ext n) where
--   halve en@(Ext mn) =
--     case mn of
--       Nothing -> (en, en)
--       Just n -> let (x, y) = halve n in (Ext (Just x), Ext (Just y))

type Domain n = (Halvable n, Group n, Ord n)

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
  | ElemLayout !Layout !b !(NonEmpty r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (Elem b) where
  bimap f g = \case
    ElemPart z -> ElemPart (g z)
    ElemLayout ly bo rs -> ElemLayout ly bo (fmap f rs)

instance Bifoldable (Elem b) where
  bifoldr f g x = \case
    ElemPart z -> g z x
    ElemLayout _ _ rs -> foldr f x rs

instance Bitraversable (Elem b) where
  bitraverse f g = \case
    ElemPart z -> fmap ElemPart (g z)
    ElemLayout ly bo rs -> fmap (ElemLayout ly bo) (traverse f rs)

data FrameF n r = FrameF {frameBoxF :: !(Box n), frameContentF :: !r}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Frame n b z = Frame {unFrame :: FrameF n (Elem b (Frame n b z) z)}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

frame :: (Domain n, HasSize n z, HasFill n b) => Elem b (Frame n b z) z -> Frame n b z
frame = \case
  ElemPart z -> framePart z
  ElemLayout ly bo fs -> cat ly bo fs

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

frameMap :: (Domain n, HasSize n w, HasFill n b) => (z -> w) -> Frame n b z -> Frame n b w
frameMap f = frameCata (frame . fmap f . frameContentF)

frameBind :: (Domain n, HasSize n w, HasFill n b) => (z -> Frame n b w) -> Frame n b z -> Frame n b w
frameBind f = frameCata (elemBind f . frameContentF)

elemBind :: (Domain n, HasSize n w, HasFill n b) => (z -> Frame n b w) -> Elem b (Frame n b w) z -> Frame n b w
elemBind f = \case
  ElemPart z -> f z
  ElemLayout ly bo fs -> cat ly bo fs

-- -- | Represents a single "pane" in a frame.
-- data Pane n =
--     PaneEmpty
--   | PaneBorder !Layout
--   | PaneImage !V.Image
--   deriving stock (Eq)

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

-- instance HasSize (Maybe Int) Pane where
--   getWidth = \case
--     PaneEmpty -> Nothing
--     PaneBorder ly ->
--       case ly of
--         LayoutHoriz -> Nothing
--         LayoutVert -> Just

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
    ElemLayout l b as -> cat l b (fmap getSize as)

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

instance Domain n => Widenable (Box n) where
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

instance Domain n => Widenable (Frame n b z) where
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
  hcat :: b -> NonEmpty a -> a
  hcat bo (hd :| tl) = foldl' (hstack bo) hd tl
  vcat :: b -> NonEmpty a -> a
  vcat bo (hd :| tl) = foldl' (vstack bo) hd tl
  cat :: Layout -> b -> NonEmpty a -> a
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
  hcat bo = V.horizCat . intersperse bo . toList
  vcat bo = V.vertCat . intersperse bo . toList

instance (Monoid n, Ord n, HasFill n b) => Stackable b (Box n) where
  stack ly bo b1 b2 = Box defBoxAlign mempty (stack ly bo (getSize b1) (getSize b2)) mempty

-- TODO impl cat to reduce tree depth
instance (Domain n, HasSize n z, HasFill n b) => Stackable b (Frame n b z) where
  stack ly bo (Frame (FrameF b1 c1)) (Frame (FrameF b2 c2)) =
    let (b1', b2') = widen ly b1 b2
        f1' = Frame (FrameF b1' c1)
        f2' = Frame (FrameF b2' c2)
        b = stack ly bo b1' b2'
        c = ElemLayout ly bo (f1' :| [f2'])
    in  Frame (FrameF b c)

build :: (Semigroup n, Stackable c w) => (n -> Layout -> b -> c) -> (Box n -> z -> w) -> Frame n b z -> w
build g f = frameCata $ \(FrameF bx e) ->
  case e of
    ElemPart z -> f bx z
    ElemLayout ly bo ws ->
      let n = case ly of LayoutHoriz -> getHeight bx; LayoutVert -> getWidth bx
      in  cat ly (g n ly bo) ws

buildM :: (Monad m, Semigroup n, Stackable c w) => (n -> Layout -> b -> m c) -> (Box n -> z -> m w) -> Frame n b z -> m w
buildM g f = frameCataM $ \(FrameF bx e) ->
  case e of
    ElemPart z -> f bx z
    ElemLayout ly bo ws ->
      let n = case ly of LayoutHoriz -> getHeight bx; LayoutVert -> getWidth bx
      in  fmap (\c -> cat ly c ws) (g n ly bo)

hitBox :: (Semigroup n, Ord n) => V2 n -> Box n -> Bool
hitBox (V2 hr hc) (Box _ (V2 tlr tlc) (V2 nr nc) _) = hr >= tlr && hr < tlr <> nr && hc >= tlc && hc < tlc <> nc

-- | Find the innermost hit
hitFrame :: (Group n, Ord n, HasFill n b) => V2 n -> Frame n b z -> Maybe z
hitFrame initPos fr = goRoot
 where
  goRoot = flip runReader initPos $ flip frameCata fr $ \(FrameF bx con) ->
    case con of
      ElemPart z -> asks (\pos -> if hitBox pos bx then Just z else Nothing)
      ElemLayout ly bo fs -> goFold bx ly bo mempty (toList fs)
  goFold bx ly bo !off = \case
    [] -> pure Nothing
    f : fs -> do
      mz <- local (off <>) f
      case mz of
        Just _ -> pure mz
        Nothing -> do
          let off' = off <> adjustOff bx ly bo
          goFold bx ly bo off' fs
  adjustOff bx ly bo =
    let z = getFill bo
    in  case ly of
          LayoutHoriz -> V2 (invert (getWidth bx <> z)) mempty
          LayoutVert -> V2 mempty (invert (getHeight bx <> z))

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

-- data Widget m e n v = Widget
--   { widgetBox :: !(Box n)
--   , widgetCallback :: !(e -> m ())
--   , widgetView :: !v
--   }

-- instance (Applicative m, Pointed v, Num n, HasSize n v) => Pointed (Widget m e n v) where
--   point = widgetConst point

-- instance Num n => HasSize n (Widget m e n v) where
--   getWidth = getWidth . widgetBox
--   getHeight = getHeight . widgetBox
--   getSize = getSize . widgetBox

-- instance (Applicative m, Pointed v, HasSize n v) => Stackable (Widget m e n v) where
--   stack l (Widget b1 cb1 v1) (Widget b2 cb2 v2) = Widget b cb v where

-- widgetConst :: (Applicative m, Num n, HasSize n v) => v -> Widget m e n v
-- widgetConst v = Widget (box (getSize v)) (const (pure ())) (pure v)

-- buildW :: (Applicative m, Pointed v, HasSize n v) => Frame n (Widget m e n v) -> Widget m e n v
-- buildW = build id
