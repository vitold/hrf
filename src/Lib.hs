module Lib
    ( listenFile
    , initCache
    , refresh
    , readFrom
    ) where

import           System.FSNotify
import           Control.Concurrent.STM
import           Control.Concurrent
import           Control.Monad

listenFile :: TVar a -> FilePath -> (String -> a) -> IO ()
listenFile cache path decode =
  withManager $ \mgr -> do
    _ <- watchDir
      mgr
      path
      (const True)
      (refresh cache path decode)
    forever $ threadDelay 1000000

initCache :: FilePath -> (String -> a) ->  IO (TVar a)
initCache path decode = do
  fileData <- readFile path
  cache <- atomically $ newTVar (decode fileData)
  _ <- forkIO $ listenFile cache path decode
  pure cache

refresh :: TVar a -> FilePath -> (String -> a) -> Action
refresh cache path decode = const $ do
  fileData <- readFile path
  atomically $ writeTVar cache (decode fileData)
  return ()

readFrom :: TVar a -> IO a
readFrom cache = atomically $ readTVar cache
