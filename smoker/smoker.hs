import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import System.Random

waiter:: Int -> TMVar Int -> STM() 
waiter a ut = putTMVar ut a

runWaiter:: TMVar Int -> TMVar Int -> TMVar Int -> IO()
runWaiter p f t = forever $ do 
					r <- randomRIO (1,3)
					threadDelay(1000000)
					bring r p f t
						
bring:: Int-> TMVar Int -> TMVar Int -> TMVar Int -> IO()
bring 1 p f t = atomically( do waiter 0 p; waiter 1 t)
bring 2 p f t = atomically( do waiter 2 f; waiter 1 t)
bring 3 p f t = atomically( do waiter 0 p; waiter 2 f)

runSmoker:: Int -> TMVar Int -> TMVar Int -> IO()
runSmoker n a b = forever $ do
	(leftNum, rightNum) <- atomically ( do leftNum <- takeTMVar a; rightNum <- takeTMVar b; return (leftNum, rightNum) )
	putStrLn (" Smoker " ++ show n ++ ": take " ++ showUt leftNum ++ " and " ++ showUt rightNum ++ " and is now smoking.")
	
showUt:: Int -> String
showUt 0 = "paper"
showUt 1 = "tabak"
showUt 2 = "fire"

main = do
  paper <- newEmptyTMVarIO
  tabak <- newEmptyTMVarIO
  fire  <- newEmptyTMVarIO
  forkIO( runWaiter paper fire tabak)
  forkIO( runSmoker 1 paper tabak )
  forkIO( runSmoker 2 fire tabak )
  forkIO( runSmoker 3 paper fire )
