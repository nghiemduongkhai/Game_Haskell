module Game.State where

import Game.Types

-- | Vị trí Y khởi đầu của thanh đập
paddleInitialY :: Float
paddleInitialY = gameHeight / 2.0 - (paddleDefaultHeight / 2.0)

ballInitialPos :: Vec2
ballInitialPos = Vec2 (gameWidth / 2.0) (gameHeight / 2.0)

ballInitialVel :: Vec2
ballInitialVel = Vec2 350.0 300.0

-- | Trạng thái khởi đầu của game
initialState :: GameState
initialState = GameState
  { ball = Ball
      { ballPos = ballInitialPos
      , ballVel = ballInitialVel
      , ballRadius = ballDefaultRadius
      }
  , paddle1 = Paddle
      { paddleX = paddleMargin
      , paddleY = paddleInitialY
      , paddleHeight = paddleDefaultHeight
      , paddleWidth = paddleDefaultWidth
      }
  , paddle2 = Paddle
      { paddleX = gameWidth - paddleMargin - paddleDefaultWidth
      , paddleY = paddleInitialY
      , paddleHeight = paddleDefaultHeight
      , paddleWidth = paddleDefaultWidth
      }
  , scoreP1 = 0
  , scoreP2 = 0
  , gameBounds = Vec2 gameWidth gameHeight
  , lastScorer = Nothing
  , isGameOver = False
  , player1Moving = 0.0
  , player2Moving = 0.0
  }