module Utils.Logging where

import Data.Time 
import System.Console.ANSI

logInfo :: String -> IO ()
logInfo msg = do
  time <- getCurrentTime
  let vnTime = utcToLocalTime (minutesToTimeZone 420) time
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "[" ++ show vnTime ++ "] [INFO] " ++ msg
  setSGR [Reset]

logError :: String -> IO ()
logError msg = do
  time <- getCurrentTime
  let vnTime = utcToLocalTime (minutesToTimeZone 420) time
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ "[" ++ show vnTime ++ "] [ERROR] " ++ msg
  setSGR [Reset]

logLogin :: String -> IO ()
logLogin playerName = do
  time <- getCurrentTime
  let vnTime = utcToLocalTime (minutesToTimeZone 420) time
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn $ "[" ++ show vnTime ++ "] [LOGIN] Player: " ++ playerName
  setSGR [Reset]