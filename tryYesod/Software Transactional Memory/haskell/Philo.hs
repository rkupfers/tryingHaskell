module Philosophers where
 
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import System.Random
 
dropForks :: TVar Bool -> TVar Bool -> IO ()
dropForks f1 f2 = atomically $ do
				b1 <- readTVar f1
				b2 <- readTVar f2
				writeTVar f1 False
				writeTVar f2 False

takeForks :: TVar Bool -> TVar Bool -> IO ()
takeForks f1 f2 = atomically $ do
				b1 <- readTVar f1
				b2 <- readTVar f2
				if(not (b1 && b2)) then( do
				writeTVar f1 True 
				writeTVar f2 True) else retry
 
left :: Int -> [a] -> a
left  k fss = fss !! (k-1)

right :: Int -> [a] -> a
right k fss = fss !! (mod k 5)

makeForks :: IO [TVar Bool]
makeForks = forM [ 0 .. 4 ] ( \ k -> do atomically $ newTVar False )

start :: [TVar Bool] -> t -> IO [ThreadId]
start fs lock = forM [ 1 .. 5 ] $ \ k -> 
			forkIO $ forever $ do 
				putStrLn $ "Thinking " ++ show k
				threadDelay (1000000)
				takeForks (left k fs) (right k fs)
				threadDelay (1000000)
				putStrLn $ "Eating " ++ show k
				dropForks (left k fs) (right k fs)
 
main = do
	forks <- makeForks
	start forks lock
