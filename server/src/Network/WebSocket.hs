{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.WebSocket where

import Control.Concurrent.STM
import Control.Exception (finally, handle, SomeException)
import Control.Monad (replicateM) 
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Game.Logic (handleInput)
import Game.State (initialState)
import Game.Types
import qualified Network.WebSockets as WS
import System.Random (randomRIO)
import Utils.Logging (logInfo, logError, logLogin)

data ClientState = ClientState
  { csRoomId :: RoomID
  , csPlayer :: Player
  } deriving (Eq, Show)

runServer :: Int -> TVar WorldState -> IO ()
runServer port worldState = do
    logInfo $ "WebSocket server on ws://0.0.0.0:" ++ show port
    WS.runServer "0.0.0.0" port (app worldState)

app :: TVar WorldState -> WS.ServerApp
app worldState pending = do
    conn <- WS.acceptRequest pending
    finally
      (loop conn worldState Nothing)
      (disconnect conn worldState Nothing)

loop :: WS.Connection -> TVar WorldState -> Maybe ClientState -> IO ()
loop conn worldState mClientState = do 
    msg <- WS.receiveData conn :: IO LBS.ByteString

    handle (\(_ :: SomeException) -> pure ()) $ do
      
      case decode msg of
        Just (clientMsg :: ClientMessage) -> do
          newState <- handleMessage conn worldState clientMsg mClientState
          
          loop conn worldState newState

        Nothing -> do
          logError $ "Failed to decode input: " ++ show msg
          WS.sendTextData conn $ encode $ ServerError "Invalid message format"
          
          loop conn worldState mClientState

-- | Hàm trợ giúp: Reset một phòng về trạng thái 'Waiting'
--   (Dùng khi rematch hoặc có người rời đi)
resetRoom :: Room -> Room
resetRoom room = room
  { gameState = initialState       -- Reset điểm số và vị trí bóng
  , roomStatus = Waiting         -- Đặt trạng thái chờ
  , p1_wantsRematch = False    -- Reset cờ rematch
  , p2_wantsRematch = False
  }

handleMessage :: WS.Connection -> TVar WorldState -> ClientMessage -> Maybe ClientState
              -> IO (Maybe ClientState)
handleMessage conn worldState msg mClientState =
    case msg of
      ClientCreateRoom playerName -> do
        logLogin $ "Client creating room: " ++ T.unpack playerName
        
        newRoomId <- generateRoomID
        let newClient = Client conn playerName Player1
        let newRoom = Room
              { roomId = newRoomId
              , player1 = Just newClient
              , player2 = Nothing
              , gameState = initialState
              , roomStatus = Waiting
              , p1_wantsRematch = False
              , p2_wantsRematch = False
              }
        
        atomically $ modifyTVar' worldState (Map.insert newRoomId newRoom)
        
        WS.sendTextData conn $ encode $ ServerRoomCreated newRoomId
        
        pure $ Just (ClientState newRoomId Player1)

      ClientJoinRoom rId playerName -> do
        logLogin $ "Client joining room: " ++ T.unpack playerName ++ " (" ++ T.unpack rId ++ ")"
        
        joinResult <- atomically $ do
          world <- readTVar worldState
          case Map.lookup rId world of
            Nothing -> pure (Left $ ServerError "Room not found")
            Just room ->
              if isNothing (player1 room) then do
                -- Ghế P1 trống, gán vào P1
                let newClient = Client conn playerName Player1
                let updatedRoom = room { player1 = Just newClient }
                
                -- Kiểm tra xem P2 có ở đó không, nếu có -> bắt đầu game
                case player2 updatedRoom of
                  Nothing -> do -- P2 vẫn trống
                    modifyTVar' worldState (Map.insert rId updatedRoom)
                    pure (Right (Player1, Nothing)) -- Vào thành công, chờ P2
                  Just p2 -> do -- P2 đã ở đó (đang chờ)
                    let finalRoom = updatedRoom { roomStatus = Countdown }
                    modifyTVar' worldState (Map.insert rId finalRoom)
                    pure (Right (Player1, Just p2)) -- Vào thành công, báo cho P2

              else if isNothing (player2 room) then do
                -- Ghế P1 bận, ghế P2 trống, gán vào P2
                let newClient = Client conn playerName Player2
                let updatedRoom = room { player2 = Just newClient, roomStatus = Countdown }
                modifyTVar' worldState (Map.insert rId updatedRoom)
                pure (Right (Player2, player1 updatedRoom)) -- Vào thành công, báo cho P1

              else
                -- Cả 2 ghế đều bận
                pure (Left $ ServerError "Room is full")
        
        case joinResult of
          Left errorMsg -> do
            WS.sendTextData conn $ encode errorMsg
            pure mClientState
          
          Right (Player1, mP2) -> do -- Người mới là P1
            case mP2 of
              Nothing -> WS.sendTextData conn $ encode $ ServerJoinedRoom playerName "Waiting..."
              Just p2 -> do
                WS.sendTextData conn $ encode $ ServerJoinedRoom playerName (clientName p2)
                WS.sendTextData (clientConn p2) $ encode $ ServerPlayerJoined playerName
            pure $ Just (ClientState rId Player1)

          Right (Player2, mP1) -> do -- Người mới là P2
            case mP1 of
              Nothing -> WS.sendTextData conn $ encode $ ServerJoinedRoom "Waiting..." playerName
              Just p1 -> do
                WS.sendTextData conn $ encode $ ServerJoinedRoom (clientName p1) playerName
                WS.sendTextData (clientConn p1) $ encode $ ServerPlayerJoined playerName
            pure $ Just (ClientState rId Player2)

      ClientInputMove clientInput ->
        case mClientState of
          Nothing -> pure Nothing
          Just (ClientState rId player) -> do
            atomically $ modifyTVar' worldState $ Map.adjust (\room ->
              let updatedGs = handleInput clientInput player (gameState room)
              in room { gameState = updatedGs }
             ) rId
            pure mClientState

      ClientLeaveRoom -> do
        case mClientState of
          Nothing -> pure Nothing
          Just (ClientState rId player) -> do
            logInfo $ "Player leaving: " ++ show player ++ " from room " ++ T.unpack rId
            disconnect conn worldState mClientState
            pure Nothing

      ClientRematch ->
        case mClientState of
          Nothing -> pure mClientState -- Bỏ qua nếu client không ở trong phòng
          Just (ClientState rId player) -> do
            logInfo $ "Player Rematch: " ++ show player ++ " from room " ++ T.unpack rId
            
            atomically $ do
              world <- readTVar worldState
              case Map.lookup rId world of
                Nothing -> pure () -- Phòng không tồn tại
                Just room -> do
                  -- 1. Đánh dấu người chơi muốn rematch
                  let room_Marked = if player == Player1
                                    then room { p1_wantsRematch = True }
                                    else room { p2_wantsRematch = True }
                  
                  -- 2. Kiểm tra xem cả hai đã sẵn sàng chưa
                  if p1_wantsRematch room_Marked && p2_wantsRematch room_Marked
                    then do
                      -- 3. Nếu cả hai OK -> Reset phòng và bắt đầu Countdown
                      let room_Reset = (resetRoom room_Marked) { roomStatus = Countdown }
                      modifyTVar' worldState (Map.insert rId room_Reset)
                    else do
                      -- 3. Nếu chưa đủ -> Chỉ lưu lại trạng thái đã đánh dấu
                      modifyTVar' worldState (Map.insert rId room_Marked)
            
            pure mClientState

disconnect :: WS.Connection -> TVar WorldState -> Maybe ClientState -> IO ()
disconnect _conn _worldState Nothing =
  logInfo "Lobby client disconnected"

-- SỬA LỖI 1: Đổi tên 'roomId' thành 'rId' để tránh trùng lặp
disconnect _conn worldState (Just (ClientState rId player)) = do
  logInfo $ "Player disconnected: " ++ show player ++ " from room " ++ T.unpack rId
  
  -- SỬA LỖI 2: Di chuyển log ra ngoài 'atomically'
  -- 'atomically' sẽ trả về (Bool: phòng có bị xóa không?, Maybe Client: ai là người còn lại?)
  (wasDeleted, mOpponent) <- atomically $ do
    world <- readTVar worldState
    case Map.lookup rId world of -- Dùng 'rId'
      Nothing -> pure (False, Nothing) -- Phòng đã bị xóa
      Just room -> do
        -- 1. Cập nhật phòng: Gỡ người chơi hiện tại ra
        let updatedRoom = if player == Player1
                          then room { player1 = Nothing }
                          else room { player2 = Nothing }
        
        -- 2. Kiểm tra xem phòng có trống không
        if isNothing (player1 updatedRoom) && isNothing (player2 updatedRoom)
          then do
            -- 3a. Xóa phòng nếu cả 2 người rời đi
            modifyTVar' worldState (Map.delete rId)
            pure (True, Nothing) -- Phòng đã bị xóa, không còn ai
          else do
            -- 3b. Vẫn còn 1 người. Reset phòng về 'Waiting'
            let room_Reset = resetRoom updatedRoom
            modifyTVar' worldState (Map.insert rId room_Reset)
            -- Trả về người chơi còn lại
            let opponent = if player == Player1 then player2 updatedRoom else player1 updatedRoom
            pure (False, opponent) -- Phòng được reset, trả về người còn lại

  -- 3. Log (ở bên ngoài IO) dựa trên kết quả
  if wasDeleted
    then logInfo $ "Room empty, deleting: " ++ T.unpack rId
    else logInfo $ "Player left, resetting room: " ++ T.unpack rId
    
  -- 4. Thông báo cho người chơi còn lại (Opponent)
  case mOpponent of
    Just opponent -> WS.sendTextData (clientConn opponent) (encode $ ServerError "Opponent left, room reset.")
    Nothing -> pure ()

generateRoomID :: IO RoomID
generateRoomID = T.pack . take 6 . map (charList !!) <$> replicateM 6 (randomRIO (0, 9))
  where charList = ['0'..'9']