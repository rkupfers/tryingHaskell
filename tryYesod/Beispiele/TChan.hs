import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, retry, atomically)
import Control.Concurrent
import Control.Monad

-- <<TChan
data TChan a = TChan (TVar (TVarList a))
                     (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan ::  STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

readTChan :: TChan a -> STM a
readTChan (TChan readVar _) = do
  listHead <- readTVar readVar
  head <- readTVar listHead
  case head of
    TNil -> retry
    TCons val tail -> do
        writeTVar readVar tail
        return val

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeVar) a = do
  newListEnd <- newTVar TNil
  listEnd <- readTVar writeVar
  writeTVar writeVar newListEnd
  writeTVar listEnd (TCons a newListEnd)
-- >>

atomInc x = atomically $ do 
			i <- readTVar x
			writeTVar x (i+1)

main = do
  shared <- atomically $ newTVar 0
  c <- atomically $ newTChan
  forkIO $ forM_ [1..2500] (\x ->  atomically $ writeTChan c x)
  forkIO $ forM_ [1..2500] (\x ->  atomically $ writeTChan c x)
  forkIO $ forM_ [1..2500] (\x ->  atomically $ writeTChan c x)
  forkIO $ forM_ [1..2500] (\x ->  atomically $ writeTChan c x)
  forkIO $ replicateM_ 2500 ( do atomically (readTChan c) >>=  print; atomInc shared)
  forkIO $ replicateM_ 2500 ( do atomically (readTChan c) >>=  print; atomInc shared)
  forkIO $ replicateM_ 2500 ( do atomically (readTChan c) >>=  print; atomInc shared)
  forkIO $ replicateM_ 2600 ( do atomically (readTChan c) >>=  print; atomInc shared)
  getLine
  after <- atomically $ readTVar shared
  putStrLn $ "After: " ++ show after