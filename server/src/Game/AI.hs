module Game.AI where

import Game.Types

aiMove :: GameState -> GameState
aiMove state =
  let
    p2 = paddle2 state
    ballY = vecY $ ballPos $ ball state
    paddleMid = paddleY p2 + paddleHeight p2 / 2

    -- Khoảng cách giữa bóng và trung tâm paddle
    diff = ballY - paddleMid

    -- Giới hạn tốc độ AI (pixel/frame)
    maxSpeed = 0.8  -- 0 < maxSpeed <= 1, càng nhỏ càng chậm
    moveValue
      | abs diff < 10 = 0                -- Nếu gần tâm, không di chuyển
      | diff > 0     = min maxSpeed 1.0  -- bóng phía dưới -> di chuyển xuống
      | diff < 0     = max (-maxSpeed) (-1.0) -- bóng phía trên -> di chuyển lên
      | otherwise    = 0
  in
    state { player2Moving = moveValue }
