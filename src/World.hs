module World where

--------------------
-- Global Imports --
import Linear.V2

----------
-- Code --

-- | The data structure to represent a paddle.
data Paddle = Paddle (V2 Float) (V2 Float)

-- | The data structure to represent a ball.
data Ball = Ball (V2 Float) Float

-- | The data structure that represents the world.
data World = World { lPaddle :: Paddle
                   , lScore  :: Int
                   , rPaddle :: Paddle
                   , rScore  :: Int
                   , ball    :: Ball
                   }
