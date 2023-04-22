module Tricksy.Ref
  ( Ref (..)
  , viewRef
  )
where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad ((>=>))

newtype Ref a = Ref {peelRef :: IO (STM a)}
  deriving stock (Functor)

instance Applicative Ref where
  pure = Ref . pure . pure
  liftA2 f (Ref x) (Ref y) = Ref (liftA2 (liftA2 f) x y)

instance Alternative Ref where
  empty = Ref (pure empty)
  Ref x <|> Ref y = Ref (liftA2 (<|>) x y)

viewRef :: Ref a -> IO a
viewRef = peelRef >=> atomically
