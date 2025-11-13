{-# LANGUAGE OverloadedStrings #-}

module Game.Logic where
    
import Game.Types
import Game.State (initialState, ballInitialPos) 
import Game.AI (aiMove)

-- | Hàm xử lý input từ client
handleInput :: ClientInput -> Player -> GameState -> GameState
handleInput input player state =
  let
    (key, moveValue) = case input of
      KeyDown k -> (k, if k == "w" || k == "ArrowUp" then -1.0 else if k == "s" || k == "ArrowDown" then 1.0 else 0)
      KeyUp k   -> (k, 0.0)

    isP1Key = key == "w" || key == "s"
    isP2Key = key == "ArrowUp" || key == "ArrowDown"
  in
    case player of
      Player1 | isP1Key -> state { player1Moving = moveValue }
      Player2 | isP2Key -> state { player2Moving = moveValue }
      _                 -> state -- Phím không liên quan hoặc người chơi không đúng

-- | Hàm cập nhật chính, chạy mỗi "tick" của game
updateGame :: Float -> Float -> Bool -> GameState -> GameState
updateGame dt randomY isAI state
  | isGameOver state = state
  | otherwise =
      let
          -- AI di chuyển paddle 2 nếu bật AI
          stateAfterAI = if isAI then aiMove state else state
          -- Di chuyển paddle theo input
          movedPaddlesState = movePaddles dt stateAfterAI
          -- Di chuyển bóng
          movedBallState = moveBall dt movedPaddlesState
          -- Kiểm tra va chạm
          collidedState = checkCollisions movedBallState
          -- Kiểm tra ghi điểm
          scoredState = checkScoring collidedState randomY
          -- Kiểm tra điều kiện thắng
          finalState = checkWinCondition scoredState
      in finalState

-- | Di chuyển thanh đập
movePaddles :: Float -> GameState -> GameState
movePaddles dt state =
  let
    p1 = paddle1 state
    p2 = paddle2 state
    (Vec2 _ h) = gameBounds state

    -- Tính vị trí Y mới cho P1
    newY1 = paddleY p1 + (player1Moving state * paddleSpeed * dt)
    -- Giới hạn trong màn hình
    clampedY1 = max 0 (min (h - paddleHeight p1) newY1)

    -- Tính vị trí Y mới cho P2
    newY2 = paddleY p2 + (player2Moving state * paddleSpeed * dt)
    clampedY2 = max 0 (min (h - paddleHeight p2) newY2)

  in
    state
      { paddle1 = p1 { paddleY = clampedY1 }
      , paddle2 = p2 { paddleY = clampedY2 }
      }

-- | Di chuyển bóng
moveBall :: Float -> GameState -> GameState
moveBall dt state =
  let
    currentBall = ball state
    oldPos = ballPos currentBall
    oldVel = ballVel currentBall
    newPos = Vec2
      (vecX oldPos + vecX oldVel * dt)
      (vecY oldPos + vecY oldVel * dt)
    newBall = currentBall { ballPos = newPos }
  in
    state { ball = newBall }

-- | Kiểm tra va chạm (Tường và Thanh đập)
checkCollisions :: GameState -> GameState
checkCollisions state =
  let
    currentBall = ball state
    pos = ballPos currentBall
    vel = ballVel currentBall
    radius = ballRadius currentBall
    (Vec2 _ h) = gameBounds state
    
    p1 = paddle1 state
    p2 = paddle2 state

    -- 1. Va chạm tường trên/dưới
    (newPos, newVel) =
      if (vecY pos - radius <= 0 && vecY vel < 0) || (vecY pos + radius >= h && vecY vel > 0)
      then (pos { vecY = max radius (min (h - radius) (vecY pos)) }, vel { vecY = -vecY vel })
      else (pos, vel)
      
    -- 2. Va chạm thanh đập P1 (Bên trái)
    (finalPos, finalVel) =
      if vecX newVel < 0 && -- Đang đi về bên trái
         (vecX newPos - radius) <= (paddleX p1 + paddleWidth p1) && -- Vượt qua mặt phải của thanh P1
         (vecX newPos + radius) > paddleX p1 && -- Bóng không hoàn toàn ở sau thanh P1
         (vecY newPos + radius) >= paddleY p1 && -- Đáy bóng > đỉnh thanh P1
         (vecY newPos - radius) <= (paddleY p1 + paddleHeight p1) -- Đỉnh bóng < đáy thanh P1
      then
        let
          -- Tính toán độ nảy (tùy thuộc vào vị trí va chạm trên thanh)
          relativeIntersectY = (paddleY p1 + (paddleHeight p1 / 2)) - vecY newPos
          normalizedIntersectY = relativeIntersectY / (paddleHeight p1 / 2)
          bounceAngle = normalizedIntersectY * (pi / 3) -- Nảy tối đa 60 độ
          ballSpeed = sqrt (vecX vel * vecX vel + vecY vel * vecY vel)
          
          newVelX = ballSpeed * cos bounceAngle
          newVelY = ballSpeed * (-sin bounceAngle)
        in
          (newPos { vecX = paddleX p1 + paddleWidth p1 + radius }, Vec2 newVelX newVelY)
      else
        (newPos, newVel)
        
    -- 3. Va chạm thanh đập P2 (Bên phải)
    (finalPos', finalVel') =
      if vecX finalVel > 0 && -- Đang đi về bên phải
         (vecX finalPos + radius) >= paddleX p2 && -- Vượt qua mặt trái của thanh P2
         (vecX finalPos - radius) < (paddleX p2 + paddleWidth p2) &&
         (vecY finalPos + radius) >= paddleY p2 &&
         (vecY finalPos - radius) <= (paddleY p2 + paddleHeight p2)
      then
        let
          relativeIntersectY = (paddleY p2 + (paddleHeight p2 / 2)) - vecY finalPos
          normalizedIntersectY = relativeIntersectY / (paddleHeight p2 / 2)
          bounceAngle = normalizedIntersectY * (pi / 3)
          ballSpeed = sqrt (vecX finalVel * vecX finalVel + vecY finalVel * vecY finalVel)
          
          newVelX = (-ballSpeed) * cos bounceAngle -- Đảo hướng X
          newVelY = ballSpeed * (-sin bounceAngle)
        in
          (finalPos { vecX = paddleX p2 - radius }, Vec2 newVelX newVelY)
      else
        (finalPos, finalVel)
        
  in
    state { ball = currentBall { ballPos = finalPos', ballVel = finalVel' } }


-- | Kiểm tra ghi điểm
checkScoring :: GameState -> Float -> GameState
checkScoring state randomY =
  let
    pos = ballPos (ball state)
    radius = ballRadius (ball state)
    (Vec2 w _) = gameBounds state
  in
    if vecX pos - radius <= 0
    then resetBall Player2 (state { scoreP2 = scoreP2 state + 1 }) randomY
    else if vecX pos + radius >= w
    then resetBall Player1 (state { scoreP1 = scoreP1 state + 1 }) randomY
    else state

-- | Reset bóng
resetBall :: Player -> GameState -> Float -> GameState
resetBall scorer state randomY =
  let
    directionX = if scorer == Player1 then -1.0 else 1.0
    baseSpeedX = abs (vecX (ballVel (ball initialState)))
    baseSpeedY = abs (vecY (ballVel (ball initialState)))

    -- TẠO VẬN TỐC MỚI (SỬ DỤNG randomY)
    newVel = Vec2
      (directionX * baseSpeedX)   -- Hướng X về phía người ghi điểm
      (randomY * baseSpeedY)      -- Hướng Y ngẫu nhiên (dựa trên randomY)
    newBall = (ball initialState) { ballPos = ballInitialPos, ballVel = newVel }
  in
    state { ball = newBall, lastScorer = Just scorer }

-- | Kiểm tra thắng
checkWinCondition :: GameState -> GameState
checkWinCondition state =
  if scoreP1 state >= maxScore || scoreP2 state >= maxScore
  then state { isGameOver = True }
  else state