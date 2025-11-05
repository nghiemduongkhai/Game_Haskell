{-# LANGUAGE OverloadedStrings #-}

module Network.API where

import Network.Wai (Application)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai.Middleware.Cors (simpleCors)
import WaiAppStatic.Types (unsafeToPiece, MaxAge(..))
import Control.Concurrent.MVar (MVar, readMVar)

runHttpServer :: MVar Bool -> IO ()
runHttpServer reloadFlag = do
  putStrLn "Serving static UI at http://0.0.0.0:1101"

  let warpSettings = setHost "0.0.0.0" $ setPort 1101 defaultSettings

      -- đọc reloadFlag mỗi request
      app :: Application
      app req respond = do
        needReload <- readMVar reloadFlag
        let maxAge = if needReload then NoMaxAge else MaxAgeSeconds 15
            settings = (defaultWebAppSettings "static")
                { ssIndices = [unsafeToPiece "index.html"]
                , ssMaxAge  = maxAge
                }
        staticApp settings req respond

  runSettings warpSettings $ simpleCors app
