module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
 
main = do shared <- atomically $ newTVar 0
          before <- atomically $ readTVar shared
          putStrLn $ "Before: " ++ show before
          forkIO $ replicateM_ 2500 (atomically $ atomInc shared)
          forkIO $ replicateM_ 2500 (atomically $ atomInc shared)
          forkIO $ replicateM_ 2500 (atomically $ atomInc shared)
          forkIO $ replicateM_ 2500 (atomically $ atomInc shared)
          getLine
          after <- atomically $ readTVar shared
          putStrLn $ "After: " ++ show after
 
atomInc :: Num a => TVar a -> STM ()
atomInc x = do 
			i <- readTVar x
			writeTVar x (i+1)