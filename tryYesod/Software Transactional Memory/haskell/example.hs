{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, retry, atomically)
import Control.Concurrent
import Control.Monad           
import Yesod

data HelloWorld = HelloWorld {
    counter :: TVar Integer
}

mkYesod "HelloWorld" [parseRoutes|
/ CounterR GET
|]

instance Yesod HelloWorld

incCount ::  TVar Integer -> IO ()
incCount counter = atomically $ do 
								i <- readTVar counter
								writeTVar counter (i+1)

getCounterR :: Handler RepHtml
getCounterR = do 
    yesod <- getYesod
    liftIO $ incCount $ counter yesod
    test  <- liftIO $ ( do atomically (readTVar (counter yesod)))
    defaultLayout [whamlet|Hello World #{test}|]

main :: IO ()
main = do
    counter <- atomically $ newTVar 0
    warpDebug 3000 $ HelloWorld { counter = counter }