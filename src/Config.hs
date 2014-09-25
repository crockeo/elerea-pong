module Config where

--------------------
-- Global Imports --
import Linear.V2

----------
-- Code --

-- | The speed of the paddle.
paddleSpeed :: Float
paddleSpeed = 100

-- | The minimum speed of the paddle.
minSpeed :: Float
minSpeed = 1

-- | The paddle width.
paddleWidth :: Float
paddleWidth = 3

-- | The paddle height.
paddleHeight :: Float
paddleHeight = 10

-- | The paddle size.
paddleSize :: V2 Float
paddleSize = V2 paddleWidth paddleHeight

-- | The speed of the ball.
ballSpeedRaw :: V2 Float
ballSpeedRaw = V2 20 20

-- | The radius of the ball.
ballRadius :: Float
ballRadius = 2

-- | Scaling the ball by this value whenever it bounces.
ballScale :: Float
ballScale = 1.05

-- | The render detail of the ball.
renderDetail :: Float
renderDetail = ballRadius * 5
