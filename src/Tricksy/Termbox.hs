-- | See https://hackage.haskell.org/package/termbox-banana-1.0.0/docs/src/Termbox.Banana.html
module Tricksy.Termbox where

import Control.Monad.IO.Class (liftIO)
import Termbox qualified as T
import Tricksy.Internal (Behavior, Events)
import Tricksy.Monad (ResM, runResM)

data Inputs = Inputs
  { inpInitSize :: !T.Size
  , inpKeyE :: !(Events T.Key)
  , inpResizeE :: !(Events T.Size)
  , inpMouseE :: !(Events T.Mouse)
  }

data Outputs a = Outputs
  { outSceneB :: !(Behavior T.Scene)
  , outDoneE :: !(Events a)
  }

createInputs :: T.Size -> Inputs
createInputs = error "TODO"

run :: (Inputs -> ResM (Outputs a)) -> IO (Either T.InitError a)
run program = T.run $ runResM $ do
  initSize <- liftIO T.getSize
  let inp = createInputs initSize
  _out <- program inp
  error "TODO"
