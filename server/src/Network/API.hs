{-# LANGUAGE OverloadedStrings #-}

module Network.API where

import Network.Wai (Application)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai.Middleware.Cors (simpleCors)
import WaiAppStatic.Types (unsafeToPiece, MaxAge(..))
import Control.Concurrent.MVar (MVar, readMVar)
import Utils.Logging (logInfo)         -- Import Logging

runHttpServer :: MVar Bool -> Int -> FilePath -> IO ()
runHttpServer reloadFlag httpPort staticPath = do
  logInfo $ "Serving static UI at http://0.0.0.0:" ++ show httpPort

  let warpSettings = setHost "0.0.0.0" $ setPort httpPort defaultSettings

      -- đọc reloadFlag mỗi request
      app :: Application
      app req respond = do
        needReload <- readMVar reloadFlag
        let maxAge = if needReload then NoMaxAge else MaxAgeSeconds 15
            settings = (defaultWebAppSettings staticPath)
                { ssIndices = [unsafeToPiece "login.html"]
                , ssMaxAge  = maxAge
                }
        staticApp settings req respond

  runSettings warpSettings $ simpleCors app
