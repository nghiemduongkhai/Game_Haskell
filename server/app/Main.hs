{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Main where

import Control.Concurrent (forkIO, newMVar, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, forM_, void, replicateM)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Game.Logic (updateGame)
import Game.Types 
import Network.API (runHttpServer)
import Network.WebSocket (runServer)
import qualified Network.WebSockets as WS
import System.Random (randomRIO) 
import Utils.Config (loadConfig, ServerConfig(..))
import Utils.Logging (logInfo)

main :: IO ()
main = do
    config <- loadConfig

    reloadFlag <- newMVar False
    -- Khởi tạo WorldState (Map rỗng)
    worldState  <- newTVarIO Map.empty

    -- Chạy HTTP server
    _ <- forkIO $ runHttpServer reloadFlag (httpPort config) (staticPath config)
    
    -- Chạy WebSocket server
    _ <- forkIO $ runServer (wsPort config) worldState
    
    -- Chạy Game Loop
    _ <- forkIO $ gameLoop worldState

    logInfo "Servers running. Press Ctrl+C or Enter to stop."
    _ <- getLine
    logInfo "Stopping servers..."

-- | Vòng lặp game chính 
gameLoop :: TVar WorldState -> IO ()
gameLoop worldState = forever $ do
    
    -- Lấy danh sách các phòng 'Playing'
    playingRooms <- filter (\r -> roomStatus r == Playing) . Map.elems
                   <$> readTVarIO worldState
    
    -- Tạo số ngẫu nhiên (cho logic reset bóng) cho mỗi phòng đang chơi
    randoms <- replicateM (length playingRooms) (randomRIO (-1.0, 1.0))
    let roomsAndRandoms = zip playingRooms randoms

    -- Cập nhật tất cả các phòng (trong 1 STM)
    atomically $ do
        forM_ roomsAndRandoms $ \(room, randomY) -> do
            -- Xác định xem player2 có phải AI không
            let isAIPlayer = case player2 room of
                               Just (Client AI _ _) -> True
                               _       -> False
            -- Cập nhật game state
            let updatedGameState = updateGame deltaTime randomY isAIPlayer (gameState room)
            -- Tạo room mới với gameState đã cập nhật
            let updatedRoom = room { gameState = updatedGameState }
            -- Kiểm tra thắng
            let finalRoom = checkWinConditionRoom updatedRoom
            -- Cập nhật vào WorldState
            modifyTVar' worldState (Map.insert (roomId finalRoom) finalRoom)
            
        -- Cập nhật các phòng 'Countdown'
        currentWorld <- readTVar worldState
        let countdownRooms = filter (\r -> roomStatus r == Countdown) (Map.elems currentWorld)
        forM_ countdownRooms $ \room -> do

            let updatedRoom = room { roomStatus = Playing }
            modifyTVar' worldState (Map.insert (roomId updatedRoom) updatedRoom)

    -- Broadcast trạng thái mới nhất cho TẤT CẢ client
    finalWorld <- readTVarIO worldState
    broadcastUpdates (Map.elems finalWorld)

    threadDelay gameTickRate

-- | Hàm trợ giúp kiểm tra thắng
checkWinConditionRoom :: Room -> Room
checkWinConditionRoom room =
    let gs = gameState room
    in
    -- Chỉ kiểm tra nếu game đang 'Playing' VÀ 'isGameOver' vừa được set
    if roomStatus room == Playing && isGameOver gs
    then 
        -- Lấy tên người thắng
        let winnerName = if scoreP1 gs > scoreP2 gs
                         then maybe "Player 1" clientName (player1 room)
                         else maybe "Player 2" clientName (player2 room)
        in room { roomStatus = GameOver winnerName }
    else room

-- | Gửi trạng thái mới nhất của phòng cho các client trong phòng đó
broadcastUpdates :: [Room] -> IO ()
broadcastUpdates rooms = forM_ rooms $ \room -> do
    -- Chuẩn bị message
    let msg = encode $ ServerUpdateState (gameState room) (roomStatus room)
    
    -- Gửi cho P1 (nếu tồn tại)
    case player1 room of
        Just p1 -> 
            case clientConn p1 of
                Human conn -> sendSafely conn msg
                AI         -> pure ()
        Nothing -> pure ()
        
    -- Gửi cho P2 (nếu tồn tại)
    case player2 room of
        Just p2 -> 
            case clientConn p2 of
                Human conn -> sendSafely conn msg
                AI         -> pure ()
        Nothing -> pure ()

-- | Gửi message một cách an toàn (bắt exception nếu client ngắt kết nối)
sendSafely :: WS.Connection -> LBS.ByteString -> IO ()
sendSafely conn msg = void (try (WS.sendTextData conn msg) :: IO (Either SomeException ()))