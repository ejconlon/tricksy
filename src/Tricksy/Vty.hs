module Tricksy.Vty
  ( Size (..)
  , Rect (..)
  , Layout (..)
  , Frame
  , frameRect
  , framePart
  , frameMap
  , frameBind
  , Pointed (..)
  , HasSize (..)
  , Positioned (..)
  , Widenable (..)
  , Stackable (..)
  , Horizontally (..)
  , Vertically (..)
  , build
  , buildM
  ) where

import Control.Monad ((<=<))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (foldl', toList)
import Data.Sequence (Seq (..))
import Graphics.Vty (Image, horizCat, horizJoin, imageHeight, imageWidth, vertCat, vertJoin)
import Termbox (Pos (..))

data Size = Size {sizeHeight :: !Int, sizeWidth :: !Int}
  deriving stock (Eq, Ord, Show)

data Rect = Rect {rectTopLeft :: !Pos, rectSize :: !Size}
  deriving stock (Eq, Ord, Show)

data Layout = LayoutHoriz | LayoutVert
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- Seqs are guaranteed nonempty by construction
data Elem r z
  = ElemPart !z
  | ElemLayout !Layout !(Seq r)
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

data FrameF r = FrameF {frameRectF :: !Rect, frameContentF :: !r}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Frame z = Frame {unFrame :: FrameF (Elem (Frame z) z)}
  deriving stock (Show)
  deriving newtype (Eq, Pointed, HasSize, Positioned)

frame :: (Pointed z, HasSize z) => Elem (Frame z) z -> Frame z
frame = \case
  ElemPart z -> framePart z
  ElemLayout l fs -> cat l fs

frameRect :: Frame z -> Rect
frameRect = frameRectF . unFrame

frameElem :: Frame z -> Elem (Frame z) z
frameElem = frameContentF . unFrame

frameCata :: (FrameF (Elem x z) -> x) -> Frame z -> x
frameCata f = go where go = f . fmap (first go) . unFrame

frameCataM :: Monad m => (FrameF (Elem x z) -> m x) -> Frame z -> m x
frameCataM f = go where go = f <=< traverse (bitraverse go pure) . unFrame

framePart :: HasSize z => z -> Frame z
framePart z = Frame (FrameF (Rect point (getSize z)) (ElemPart z))

frameMap :: (Pointed w, HasSize w) => (z -> w) -> Frame z -> Frame w
frameMap f = frameCata (frame . fmap f . frameContentF)

frameBind :: (Pointed w, HasSize w) => (z -> Frame w) -> Frame z -> Frame w
frameBind f = frameCata (elemBind f . frameContentF)

elemBind :: (Pointed w, HasSize w) => (z -> Frame w) -> Elem (Frame w) z -> Frame w
elemBind f = \case
  ElemPart z -> f z
  ElemLayout l fs -> cat l fs

class Pointed a where
  point :: a

instance Pointed Image where
  point = mempty

instance Pointed Pos where
  point = Pos 0 0

instance Pointed Size where
  point = Size 0 0

instance Pointed Rect where
  point = Rect point point

instance Pointed z => Pointed (Elem r z) where
  point = ElemPart point

instance Pointed a => Pointed (FrameF a) where
  point = FrameF point point

instance Pointed (Maybe a) where
  point = Nothing

instance Pointed a => Pointed (Either b a) where
  point = Right point

instance Pointed () where
  point = ()

class HasSize a where
  getWidth :: a -> Int
  getWidth = sizeWidth . getSize
  getHeight :: a -> Int
  getHeight = sizeHeight . getSize
  getSize :: a -> Size
  getSize a = Size (getWidth a) (getHeight a)
  {-# MINIMAL (getWidth, getHeight) | getSize #-}

instance HasSize Image where
  getWidth = imageWidth
  getHeight = imageHeight

instance HasSize Size where
  getWidth = sizeWidth
  getHeight = sizeHeight
  getSize = id

instance HasSize Rect where
  getSize = rectSize

instance (HasSize r, HasSize z) => HasSize (Elem r z) where
  getSize = \case
    ElemPart z -> getSize z
    ElemLayout l as -> cat l (fmap getSize (toList as))

instance HasSize a => HasSize (FrameF a) where
  getSize = getSize . frameRectF

instance HasSize a => HasSize (Maybe a) where
  getWidth = maybe 0 getWidth
  getHeight = maybe 0 getHeight
  getSize = maybe point getSize

instance (HasSize a, HasSize b) => HasSize (Either b a) where
  getWidth = either getWidth getWidth
  getHeight = either getHeight getHeight
  getSize = either getSize getSize

instance HasSize () where
  getWidth = const 0
  getHeight = const 0
  getSize = const point

class Positioned a where
  position :: a -> Pos
  translate :: Pos -> a -> a

instance Positioned Pos where
  position = id
  translate (Pos r1 c1) (Pos r2 c2) = Pos (r1 + r2) (c1 + c2)

instance Positioned Rect where
  position = rectTopLeft
  translate byPos (Rect tl sz) = Rect (translate byPos tl) sz

instance Positioned (FrameF z) where
  position = position . frameRectF
  translate byPos (FrameF (Rect pos sz) con) = FrameF (Rect (translate byPos pos) sz) con

boundingBox :: (Positioned a, HasSize a) => a -> Rect
boundingBox a = Rect (position a) (getSize a)

class HasSize a => Widenable a where
  hwiden :: Int -> a -> a
  hwiden = widen LayoutHoriz
  vwiden :: Int -> a -> a
  vwiden = widen LayoutVert
  widen :: Layout -> Int -> a -> a
  widen = \case LayoutHoriz -> hwiden; LayoutVert -> vwiden
  {-# MINIMAL widen | (hwiden, vwiden) #-}

pwiden :: Widenable a => Layout -> Pos -> a -> a
pwiden l (Pos r c) = widen l (case l of LayoutHoriz -> r; LayoutVert -> c)

instance Widenable Size where
  widen l x (Size r c) =
    case l of
      LayoutHoriz -> Size r (x + c)
      LayoutVert -> Size (r + x) c

instance Widenable Rect where
  widen l x (Rect p s) = Rect p (widen l x s)

instance Widenable a => Widenable (FrameF a) where
  -- widen l x (Frame (FrameF r s)) = Frame (FrameF _ _)
  widen = undefined

class Pointed a => Stackable a where
  hstack :: a -> a -> a
  hstack = stack LayoutHoriz
  vstack :: a -> a -> a
  vstack = stack LayoutVert
  hcat :: Foldable f => f a -> a
  hcat = foldl' hstack point
  vcat :: Foldable f => f a -> a
  vcat = foldl' vstack point
  stack :: Layout -> a -> a -> a
  stack = \case LayoutHoriz -> hstack; LayoutVert -> vstack
  cat :: Foldable f => Layout -> f a -> a
  cat = \case LayoutHoriz -> hcat; LayoutVert -> vcat
  {-# MINIMAL stack | (hstack, vstack) #-}

instance Stackable Size where
  stack l (Size h1 w1) (Size h2 w2) =
    case l of
      LayoutHoriz -> Size (max h1 h2) (w1 + w2)
      LayoutVert -> Size (h1 + h2) (max w1 w2)

instance Stackable Image where
  hstack = horizJoin
  vstack = vertJoin
  hcat = horizCat . toList
  vcat = vertCat . toList

instance Stackable Rect where
  stack l (Rect p1 s1) (Rect p s2) = Rect p1 (stack l s1 (pwiden l p s2))

instance (Pointed z, HasSize z) => Stackable (Frame z) where
  stack l f1 f2 = undefined

newtype Horizontally a = Horizontally {unHorizontally :: a}

instance Stackable a => Semigroup (Horizontally a) where
  Horizontally x <> Horizontally y = Horizontally (hstack x y)

instance Stackable a => Monoid (Horizontally a) where
  mempty = Horizontally point
  mappend = (<>)
  mconcat = Horizontally . hcat . fmap unHorizontally

newtype Vertically a = Vertically {unVertically :: a}

instance Stackable a => Semigroup (Vertically a) where
  Vertically x <> Vertically y = Vertically (vstack x y)

instance Stackable a => Monoid (Vertically a) where
  mempty = Vertically point
  mappend = (<>)
  mconcat = Vertically . vcat . fmap unVertically

build :: Stackable w => (z -> w) -> Frame z -> w
build f = frameCata $ \(FrameF _ e) ->
  case e of
    ElemPart z -> f z
    ElemLayout l ws -> cat l ws

buildM :: (Monad m, Stackable w) => (z -> m w) -> Frame z -> m w
buildM f = frameCataM $ \(FrameF _ e) ->
  case e of
    ElemPart z -> f z
    ElemLayout l ws -> pure (cat l ws)
