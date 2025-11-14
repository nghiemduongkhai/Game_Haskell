module Game.AI where

import Game.Types

-- | AI di chuyển paddle 2 theo bóng
aiMove :: GameState -> GameState
aiMove state =
  let
    p2 = paddle2 state
    ballY = vecY $ ballPos $ ball state
    paddleMid = paddleY p2 + paddleHeight p2 / 2

    diff = ballY - paddleMid

    -- Giới hạn tốc độ AI (pixel/frame)
    maxSpeed = 400  -- tối đa 300 pixel/giây
    moveValue
      | abs diff < 10 = 0                       -- Nếu gần trung tâm, không di chuyển
      | diff > 0     = min 1 (diff / maxSpeed)  -- bóng phía dưới
      | diff < 0     = max (-1) (diff / maxSpeed) -- bóng phía trên
      | otherwise    = 0
  in
    state { player2Moving = moveValue }
