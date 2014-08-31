{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, retry, atomically)
import Control.Concurrent
import Control.Monad           
import Yesod

data HelloWorld = HelloWorld {
    konto1 :: TVar Integer,
	konto2 :: TVar Integer
}

mkYesod "HelloWorld" [parseRoutes|
/ CounterR GET
|]

instance Yesod HelloWorld

trans::  TVar Integer -> TVar Integer-> Integer -> IO ()
trans k1 k2 betrag = atomically $ do 
								zu <- readTVar k1
								von <- readTVar k2
								writeTVar k1 (zu+betrag)
								writeTVar k2 (von-betrag)

getCounterR :: Handler Html
getCounterR = do 
    yesod <- getYesod
    liftIO $ trans (konto1 yesod) (konto2 yesod) 5
    var1  <- liftIO $ ( do atomically (readTVar (konto1 yesod)));
	var2  <- liftIO $ ( do atomically (readTVar (konto2 yesod)))
    defaultLayout [whamlet|Konto1: #{var1}|Konto2 #{var2}|]

main :: IO ()
main = do
    k1 <- atomically $ newTVar 100;
	k2 <- atomically $ newTVar 200
    warp 3000 $ HelloWorld { konto1 = k1, konto2 = k2}