module Main
  ( main
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Tricksy.Cache (defaultCacheHandler, runCache)
import Tricksy.Monad (scoped)
import Tricksy.Ref (viewRef)
import Tricksy.Time (threadDelayDelta, timeDeltaFromFracSecs)

testCache :: TestTree
testCache = testCase "cache" $ do
  let han = defaultCacheHandler 0
  ref <- newIORef (1 :: Int)
  let act = putStrLn "fetching" *> readIORef ref
      ttl = timeDeltaFromFracSecs (0.05 :: Double)
  -- Test with requests
  scoped $ \_ -> do
    (readCache, dispose) <- runCache ttl han act
    liftIO $ do
      v1 <- viewRef readCache
      v1 @?= 1
      writeIORef ref 2
      v2 <- viewRef readCache
      v2 @?= 1
      threadDelayDelta ttl
      threadDelayDelta ttl
      v3 <- viewRef readCache
      v3 @?= 2
      writeIORef ref 3
      dispose
      threadDelayDelta ttl
      threadDelayDelta ttl
      v4 <- viewRef readCache
      v4 @?= 2
  -- Test without requests
  scoped $ \_ -> do
    (readCache, dispose) <- runCache ttl han act
    liftIO $ do
      dispose
      v1 <- viewRef readCache
      v1 @?= 0

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMain $
    testGroup
      "Tricksy"
      [ testCache
      ]
