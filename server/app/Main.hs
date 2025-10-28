module Main where
import Game.Logic

main :: IO ()
main = do
    putStrLn "Starting Pong server..."
    startServer
