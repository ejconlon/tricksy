{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Tricksy.Graph where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)

-- import Tricksy.Internal qualified as TI

newtype GraphEnv = GraphEnv
  { geCap :: Int
  }

defaultCap :: Int
defaultCap = 1000

newGraphEnv :: IO GraphEnv
newGraphEnv = pure (GraphEnv defaultCap)

newtype Events a = Events {unEvents :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype GraphM a = GraphM {unGraphM :: ReaderT GraphEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader GraphEnv, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, PrimMonad)

onceE :: IO a -> GraphM (Events a)
onceE = undefined
