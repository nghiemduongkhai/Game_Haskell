{-# LANGUAGE OverloadedStrings #-}

module Utils.Config (ServerConfig(..), loadConfig) where

import System.Environment (lookupEnv)

data ServerConfig = ServerConfig
  { httpPort   :: Int      
  , wsPort     :: Int      
  , staticPath :: FilePath 
  } deriving (Show)

loadConfig :: IO ServerConfig
loadConfig = do
    mHttp <- lookupEnv "HTTP_PORT"
    mWs   <- lookupEnv "WS_PORT"
    let httpPortVal   = maybe 8080 read mHttp
        wsPortVal     = maybe 8081 read mWs
        staticPathVal = "static"

    pure ServerConfig
        { httpPort   = httpPortVal
        , wsPort     = wsPortVal
        , staticPath = staticPathVal
        }