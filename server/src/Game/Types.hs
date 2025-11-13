{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types where

import Data.Aeson (FromJSON, ToJSON, (.:), withObject, parseJSON)
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS

-- =============================================================================
--  CÁC KIỂU DỮ LIỆU GAME PONG CƠ BẢN
-- =============================================================================

-- Định nghĩa các hằng số của game
gameWidth :: Float
gameWidth = 640.0   

gameHeight :: Float
gameHeight = 400.0

paddleMargin :: Float
paddleMargin = 15.0

paddleDefaultHeight :: Float
paddleDefaultHeight = 120.0

paddleDefaultWidth :: Float
paddleDefaultWidth = 10.0

ballDefaultRadius :: Float
ballDefaultRadius = 10.0

-- Tốc độ di chuyển của thanh đập (pixel/giây)
paddleSpeed :: Float
paddleSpeed = 450.0

-- Điểm số tối đa để thắng
maxScore :: Int
maxScore = 3

-- Tần suất cập nhật game (60 FPS)
gameTickRate :: Int
gameTickRate = 1000000 `div` 60 -- microgiây (1/60 giây)

-- Delta time (thời gian mỗi frame)
deltaTime :: Float
deltaTime = 1.0 / 60.0

data Vec2 = Vec2
  { vecX :: Float
  , vecY :: Float
  } deriving (Show, Eq, Generic)

instance ToJSON Vec2  
instance FromJSON Vec2

data Ball = Ball
  { ballPos :: Vec2
  , ballVel :: Vec2
  , ballRadius :: Float
  } deriving (Show, Generic)

instance ToJSON Ball
instance FromJSON Ball

data Paddle = Paddle
  { paddleX :: Float
  , paddleY    :: Float
  , paddleHeight :: Float
  , paddleWidth  :: Float
  } deriving (Show, Generic)

instance ToJSON Paddle
instance FromJSON Paddle

data Player = Player1 | Player2
  deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

data GameState = GameState
  { ball      :: Ball
  , paddle1   :: Paddle
  , paddle2   :: Paddle
  , scoreP1   :: Int
  , scoreP2   :: Int
  , gameBounds :: Vec2
  , lastScorer :: Maybe Player
  , isGameOver :: Bool
  , player1Moving :: Float
  , player2Moving :: Float
  } deriving (Show, Generic)

instance ToJSON GameState
instance FromJSON GameState


-- =============================================================================
--  CÁC KIỂU DỮ LIỆU MỚI CHO HỆ THỐNG PHÒNG
-- =============================================================================

type RoomID = Text

data Client = Client
  { clientConn   :: WS.Connection
  , clientName   :: Text
  , clientPlayer :: Player
  }
-- (Client không cần ToJSON/FromJSON vì nó không bao giờ được gửi qua mạng)

data RoomStatus
  = Waiting
  | Countdown
  | Playing
  | GameOver Text
  deriving (Show, Eq, Generic) -- SỬA LẠI: XÓA ToJSON

instance ToJSON RoomStatus   -- THÊM DÒNG NÀY

data Room = Room
  { roomId     :: RoomID
  , player1    :: Maybe Client
  , player2    :: Maybe Client
  , gameState  :: GameState
  , roomStatus :: RoomStatus
  , p1_wantsRematch :: Bool
  , p2_wantsRematch :: Bool
  }

type WorldState = Map RoomID Room

-- =============================================================================
--  THÔNG ĐIỆP GIAO TIẾP (CLIENT <-> SERVER)
-- =============================================================================


data ClientMessage
  = ClientCreateRoom { c_playerName :: Text }
  | ClientJoinRoom   { c_roomId :: RoomID, c_playerName :: Text }
  | ClientInputMove  { c_input :: ClientInput }
  | ClientLeaveRoom
  | ClientRematch
  deriving (Show, Generic)

data ServerMessage
  = ServerRoomCreated { s_roomId :: RoomID }
  | ServerJoinedRoom  { s_p1_name :: Text, s_p2_name :: Text }
  | ServerPlayerJoined { s_p2_name :: Text }
  | ServerUpdateState { s_gameState :: GameState, s_roomStatus :: RoomStatus }
  | ServerError       { s_errorMsg :: Text }
  deriving (Show, Generic)

instance ToJSON ServerMessage

data ClientInput = KeyDown Text | KeyUp Text
  deriving (Show, Generic) 

instance ToJSON ClientInput

-- =============================================================================
--  PARSER TÙY CHỈNH CHO CÁC THÔNG ĐIỆP
-- =============================================================================

instance FromJSON ClientInput where
  parseJSON = withObject "ClientInput" $ \v -> do
    inputType <- v .: "type" :: Parser Text
    key <- v .: "key" :: Parser Text
    case inputType of
      "keydown" -> pure (KeyDown key)
      "keyup"   -> pure (KeyUp key)
      _         -> fail "Unknown input type"

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \v -> do
    action <- v .: "action" :: Parser Text
    case action of
      "createRoom" -> ClientCreateRoom <$> v .: "playerName"
      "joinRoom"   -> ClientJoinRoom <$> v .: "roomId" <*> v .: "playerName"
      "input"      -> ClientInputMove <$> v .: "data"
      "LeaveRoom"  -> pure ClientLeaveRoom
      "Rematch"    -> pure ClientRematch
      _ -> fail "Unknown client action"