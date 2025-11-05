{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket where

import System.FSNotify
import Control.Concurrent (forkIO, threadDelay, MVar, modifyMVar_)
import Control.Monad (forever)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

runServer :: Int -> MVar Bool -> IO ()
runServer port reloadFlag = do
    putStrLn $ "WebSocket server on ws://0.0.0.0:" ++ show port
    WS.runServer "0.0.0.0" port (app reloadFlag)

app :: MVar Bool -> WS.ServerApp
app reloadFlag pending = do
    conn <- WS.acceptRequest pending
    putStrLn "Client connected!"
    _ <- forkIO $ watchStatic reloadFlag conn
    loop conn

loop :: WS.Connection -> IO ()
loop conn = forever $ do
    msg <- WS.receiveData conn :: IO T.Text
    TIO.putStrLn $ "Received: " <> msg
    WS.sendTextData conn ("Echo: " <> msg)

watchStatic :: MVar Bool -> WS.Connection -> IO ()
watchStatic reloadFlag conn = withManager $ \mgr -> do
    _ <- watchDir mgr "static" (const True) $ \_ -> do
        WS.sendTextData conn ("reload" :: T.Text)
        putStrLn "File changed, reload sent!"
        modifyMVar_ reloadFlag (const (return True))
    forever $ threadDelay 1000000
