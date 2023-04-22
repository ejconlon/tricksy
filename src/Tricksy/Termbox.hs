-- | See https://hackage.haskell.org/package/termbox-banana-1.0.0/docs/src/Termbox.Banana.html
module Tricksy.Termbox where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Void (Void)
import Termbox qualified as T
import Tricksy.Internal (Behavior, Events, repeatE, ringBufferE)
import Tricksy.Monad (ResM, runResM)
import Tricksy.Ring (Next)

defaultCap :: Int
defaultCap = 1000

type TermEvent = T.Event Void

data Inputs = Inputs
  { inpInitSize :: !T.Size
  , inpTermEvents :: !(Events (Next TermEvent))
  }

data Outputs a = Outputs
  { outSceneB :: !(Behavior T.Scene)
  , outDoneE :: !(Events a)
  }

createInputs :: T.Size -> IO Inputs
createInputs sz = do
  let termEvs = repeatE T.poll & ringBufferE defaultCap
  pure (Inputs sz termEvs)

run :: (Inputs -> ResM (Outputs a)) -> IO (Either T.InitError a)
run program = T.run $ runResM $ do
  initSize <- liftIO T.getSize
  inp <- liftIO (createInputs initSize)
  _out <- program inp
  error "TODO"
