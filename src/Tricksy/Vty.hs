module Tricksy.Vty where

-- ( Layout (..)
-- , V2 (..)
-- , Align (..)
-- , BoxAlign
-- , Box (..)
-- , Frame
-- , frameBox
-- , framePart
-- , frameMap
-- , frameBind
-- , Pointed (..)
-- , HasSize (..)
-- , Widenable (..)
-- , Stackable (..)
-- , Horizontally (..)
-- , Vertically (..)
-- , build
-- , buildM
-- , hitFrame
-- , Widget (..)
-- , widgetConst
-- -- , buildW
-- )
-- where

-- import Data.Sequence (Seq (..))

import Control.Exception (bracket)
import Control.Monad (void, (<=<))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (fold, foldl', toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (First (..))
import Data.Semigroup (Semigroup (..))
import Graphics.Vty qualified as V
import Lens.Micro (Lens', lens)

data V2 n = V2 {v2x :: !n, v2y :: !n}
  deriving stock (Eq, Ord, Show)

data Layout = LayoutHoriz | LayoutVert
  deriving stock (Eq, Ord, Show, Enum, Bounded)

v2Dim :: Layout -> V2 n -> n
v2Dim = \case LayoutHoriz -> v2x; LayoutVert -> v2y

data Align = AlignBegin | AlignMiddle | AlignEnd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data BoxAlign = BoxAlign {baHoriz :: !Align, baVert :: !Align}
  deriving stock (Eq, Ord, Show)

data Box n = Box
  { boxAlign :: !BoxAlign
  , boxTopLeftOff :: !(V2 n)
  , boxContentSize :: !(V2 n)
  , boxBotRightOff :: !(V2 n)
  }
  deriving stock (Eq, Ord, Show)

box :: Num n => V2 n -> Box n
box sz = Box point point sz point

data Elem r z
  = ElemPart !z
  | ElemLayout !Layout !(NonEmpty r)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor Elem where
  bimap f g = \case
    ElemPart z -> ElemPart (g z)
    ElemLayout l rs -> ElemLayout l (fmap f rs)

instance Bifoldable Elem where
  bifoldr f g x = \case
    ElemPart z -> g z x
    ElemLayout _ rs -> foldr f x rs

instance Bitraversable Elem where
  bitraverse f g = \case
    ElemPart z -> fmap ElemPart (g z)
    ElemLayout l rs -> fmap (ElemLayout l) (traverse f rs)

data FrameF n r = FrameF {frameBoxF :: !(Box n), frameContentF :: !r}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Frame n z = Frame {unFrame :: FrameF n (Elem (Frame n z) z)}
  deriving stock (Show)
  deriving newtype (Eq, Pointed, HasSize n)

frame :: (Pointed z, HasSize n z) => Elem (Frame n z) z -> Frame n z
frame = \case
  ElemPart z -> framePart z
  ElemLayout l fs -> cat l fs

frameBox :: Frame n z -> Box n
frameBox = frameBoxF . unFrame

setFrameBox :: Frame n z -> Box n -> Frame n z
setFrameBox (Frame (FrameF _ c)) b = Frame (FrameF b c)

frameBoxL :: Lens' (Frame n z) (Box n)
frameBoxL = lens frameBox setFrameBox

frameElem :: Frame n z -> Elem (Frame n z) z
frameElem = frameContentF . unFrame

setFrameElem :: Frame n z -> Elem (Frame n z) z -> Frame n z
setFrameElem (Frame (FrameF b _)) c = Frame (FrameF b c)

frameElemL :: Lens' (Frame n z) (Elem (Frame n z) z)
frameElemL = lens frameElem setFrameElem

frameCata :: (FrameF n (Elem x z) -> x) -> Frame n z -> x
frameCata f = go where go = f . fmap (first go) . unFrame

frameCataM :: Monad m => (FrameF n (Elem x z) -> m x) -> Frame n z -> m x
frameCataM f = go where go = f <=< traverse (bitraverse go pure) . unFrame

framePart :: HasSize n z => z -> Frame n z
framePart z = Frame (FrameF (box (getSize z)) (ElemPart z))

frameMap :: (Pointed w, HasSize o w) => (z -> w) -> Frame n z -> Frame o w
frameMap f = frameCata (frame . fmap f . frameContentF)

frameBind :: (Pointed w, HasSize o w) => (z -> Frame o w) -> Frame n z -> Frame o w
frameBind f = frameCata (elemBind f . frameContentF)

elemBind :: (Pointed w, HasSize o w) => (z -> Frame o w) -> Elem (Frame o w) z -> Frame o w
elemBind f = \case
  ElemPart z -> f z
  ElemLayout l fs -> cat l fs

class Pointed a where
  point :: a

instance Pointed V.Image where
  point = mempty

instance Num n => Pointed (V2 n) where
  point = V2 0 0

instance Pointed Int where
  point = 0

instance Pointed Double where
  point = 0

instance Pointed Align where
  point = AlignBegin

instance Pointed BoxAlign where
  point = BoxAlign point point

instance Num n => Pointed (Box n) where
  point = Box point point point point

instance Pointed z => Pointed (Elem r z) where
  point = ElemPart point

instance (Pointed z, Num n) => Pointed (FrameF n z) where
  point = FrameF point point

instance Pointed (Maybe a) where
  point = Nothing

instance Pointed a => Pointed (Either b a) where
  point = Right point

instance Pointed () where
  point = ()

class (Num n, Ord n) => Divisible n where
  halve :: n -> (n, n)

instance Divisible Int where
  halve i = let x = div i 2 in (x, i - x)

instance Divisible Double where
  halve i = let x = i / 2 in (x, i - x)

class Divisible n => HasSize n a where
  getWidth :: a -> n
  getWidth = v2x . getSize
  getHeight :: a -> n
  getHeight = v2y . getSize
  getSize :: a -> V2 n
  getSize a = V2 (getWidth a) (getHeight a)
  {-# MINIMAL (getWidth, getHeight) | getSize #-}

instance HasSize Int V.Image where
  getWidth = V.imageWidth
  getHeight = V.imageHeight

instance Divisible n => HasSize n (V2 n) where
  getWidth = v2x
  getHeight = v2y
  getSize = id

instance Divisible n => HasSize n (Box n) where
  getWidth (Box _ (V2 a _) (V2 b _) (V2 c _)) = a + b + c
  getHeight (Box _ (V2 _ a) (V2 _ b) (V2 _ c)) = a + b + c

instance (HasSize n r, HasSize n z) => HasSize n (Elem r z) where
  getSize = \case
    ElemPart z -> getSize z
    ElemLayout l as -> cat l (fmap getSize as)

instance HasSize n a => HasSize n (FrameF n a) where
  getSize = getSize . frameBoxF

instance HasSize n a => HasSize n (Maybe a) where
  getWidth = maybe 0 getWidth
  getHeight = maybe 0 getHeight
  getSize = maybe point getSize

instance (HasSize n a, HasSize n b) => HasSize n (Either b a) where
  getWidth = either getWidth getWidth
  getHeight = either getHeight getHeight
  getSize = either getSize getSize

instance Divisible n => HasSize n () where
  getWidth = const 0
  getHeight = const 0
  getSize = const point

class Widenable a where
  widen :: a -> a -> (a, a)

instance Divisible n => Widenable (V2 n) where
  widen (V2 x1 y1) (V2 x2 y2) = let v = V2 (max x1 x2) (max y1 y2) in (v, v)

fitBox :: Divisible n => V2 n -> Box n -> Box n
fitBox (V2 dx dy) (Box ba@(BoxAlign bax bay) (V2 tlx tly) sz (V2 brx bry)) =
  let (tlx', brx') = case bax of
        AlignBegin -> (tlx, brx + dx)
        AlignMiddle -> let (tld, brd) = halve dx in (tlx + tld, brx + brd)
        AlignEnd -> (tlx + dx, brx)
      (tly', bry') = case bay of
        AlignBegin -> (tly, bry + dy)
        AlignMiddle -> let (tld, brd) = halve dy in (tly + tld, bry + brd)
        AlignEnd -> (tly + dy, bry)
  in  Box ba (V2 tlx' tly') sz (V2 brx' bry')

instance Divisible n => Widenable (Box n) where
  widen b1 b2 =
    let V2 x1 y1 = getSize b1
        V2 x2 y2 = getSize b2
        d1 = V2 (max x1 x2 - x1) (max y1 y2 - y1)
        d2 = V2 (max x1 x2 - x2) (max y1 y2 - y2)
        b1' = fitBox d1 b1
        b2' = fitBox d2 b2
    in  (b1', b2')

instance Divisible n => Widenable (Frame n z) where
  widen f1 f2 =
    let b1 = frameBox f1
        b2 = frameBox f2
        (b1', b2') = widen b1 b2
    in  (setFrameBox f1 b1', setFrameBox f2 b2')

class Stackable a where
  hstack :: a -> a -> a
  hstack = stack LayoutHoriz
  vstack :: a -> a -> a
  vstack = stack LayoutVert
  stack :: Layout -> a -> a -> a
  stack = \case LayoutHoriz -> hstack; LayoutVert -> vstack
  hcat :: NonEmpty a -> a
  hcat (hd :| tl) = foldl' hstack hd tl
  vcat :: NonEmpty a -> a
  vcat (hd :| tl) = foldl' vstack hd tl
  cat :: Layout -> NonEmpty a -> a
  cat = \case LayoutHoriz -> hcat; LayoutVert -> vcat
  {-# MINIMAL stack | (hstack, vstack) #-}

instance Divisible n => Stackable (V2 n) where
  stack l (V2 x1 y1) (V2 x2 y2) =
    case l of
      LayoutHoriz -> V2 (x1 + x2) (max y1 y2)
      LayoutVert -> V2 (max x1 x2) (y1 + y2)

instance Stackable V.Image where
  hstack = V.horizJoin
  vstack = V.vertJoin
  hcat = V.horizCat . toList
  vcat = V.vertCat . toList

instance Divisible n => Stackable (Box n) where
  stack l b1 b2 = Box point point (stack l (getSize b1) (getSize b2)) point

instance (Pointed z, HasSize n z) => Stackable (Frame n z) where
  stack l (Frame (FrameF b1 c1)) (Frame (FrameF b2 c2)) =
    let (b1', b2') = widen b1 b2
        f1' = Frame (FrameF b1' c1)
        f2' = Frame (FrameF b2' c2)
        b = stack l b1' b2'
        c = ElemLayout l (f1' :| [f2'])
    in  Frame (FrameF b c)

-- TODO impl cat

newtype Horizontally a = Horizontally {unHorizontally :: a}

instance Stackable a => Semigroup (Horizontally a) where
  Horizontally x <> Horizontally y = Horizontally (hstack x y)
  sconcat = Horizontally . hcat . fmap unHorizontally

instance (Pointed a, Stackable a) => Monoid (Horizontally a) where
  mempty = Horizontally point
  mappend = (<>)
  mconcat = \case
    [] -> Horizontally point
    x : xs -> sconcat (x :| xs)

newtype Vertically a = Vertically {unVertically :: a}

instance Stackable a => Semigroup (Vertically a) where
  Vertically x <> Vertically y = Vertically (vstack x y)
  sconcat = Vertically . vcat . fmap unVertically

instance (Pointed a, Stackable a) => Monoid (Vertically a) where
  mempty = Vertically point
  mappend = (<>)
  mconcat = \case
    [] -> Vertically point
    x : xs -> sconcat (x :| xs)

build :: Stackable w => (z -> w) -> Frame n z -> w
build f = frameCata $ \(FrameF _ e) ->
  case e of
    ElemPart z -> f z
    ElemLayout l ws -> cat l ws

buildM :: (Monad m, Stackable w) => (z -> m w) -> Frame n z -> m w
buildM f = frameCataM $ \(FrameF _ con) ->
  case con of
    ElemPart z -> f z
    ElemLayout l ws -> pure (cat l ws)

hitBox :: V2 n -> Box n -> Bool
hitBox = undefined

-- hitBox (Pos hr hc) (Rect (Pos tlr tlc) (Size nr nc)) = hr >= tlr && hr < tlr + nr && hc >= tlc && hc < tlc + nc

hitFrame :: V2 n -> Frame n z -> Maybe z
hitFrame pos fr = getFirst $ flip frameCata fr $ \(FrameF bx con) ->
  case con of
    ElemPart z -> First (if hitBox pos bx then Just z else Nothing)
    ElemLayout _ fs -> fold fs

data Widget m e n v = Widget
  { widgetBox :: !(Box n)
  , widgetCallback :: !(e -> m ())
  , widgetView :: !(m v)
  }

instance (Applicative m, Pointed v, HasSize n v) => Pointed (Widget m e n v) where
  point = widgetConst point

instance Divisible n => HasSize n (Widget m e n v) where
  getWidth = getWidth . widgetBox
  getHeight = getHeight . widgetBox
  getSize = getSize . widgetBox

-- instance (Applicative m, Pointed v, HasSize n v) => Stackable (Widget m e n v) where
--   stack l (Widget b1 cb1 v1) (Widget b2 cb2 v2) = Widget b cb v where
--     b =

widgetConst :: (Applicative m, HasSize n v) => v -> Widget m e n v
widgetConst v = Widget (box (getSize v)) (const (pure ())) (pure v)

-- buildW :: (Applicative m, Pointed v, HasSize n v) => Frame n (Widget m e n v) -> Widget m e n v
-- buildW = build id

showI :: Frame Int V.Image -> IO ()
showI fr = do
  let im = build id fr
      pic = V.picForImage im
  cfg <- V.standardIOConfig
  bracket (V.mkVty cfg) V.shutdown $ \vty -> do
    V.update vty pic
    void (V.nextEvent vty)

testI :: Frame Int V.Image
testI =
  let at = V.defAttr
  in  hstack
        (vstack (framePart (V.string at "hello")) (framePart (V.string at "world")))
        (framePart (V.string at ":)"))
