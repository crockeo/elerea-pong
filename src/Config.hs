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
