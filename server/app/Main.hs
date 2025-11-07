{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, newMVar)
import Game.Logic (startServer)
import Network.API (runHttpServer)
import Network.WebSocket (runServer)

main :: IO ()
main = do
    putStrLn "Starting Haskell Pong Test..."
    startServer
    reloadFlag <- newMVar False

    -- Chạy HTTP server trên thread riêng
    _ <- forkIO $ runHttpServer reloadFlag
    -- Chạy WebSocket server trên thread riêng
    _ <- forkIO $ runServer 8081 reloadFlag

    putStrLn "Servers running. Press Ctrl+C or Enter to stop."
    _ <- getLine
    putStrLn "Stopping servers..."

