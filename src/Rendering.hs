module Rendering (renderWorld) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Linear.V2

-------------------
-- Local Imports --
import World

----------
-- Code --

-- | Rendering a paddle.
renderPaddle :: Paddle -> IO ()
renderPaddle (Paddle (V2 x y) (V2 w h)) = do
  renderPrimitive Quads $
    mapM_ (\(V2 px py) -> vertex $ Vertex2 (realToFrac px :: GLfloat) (realToFrac py :: GLfloat))
          [ V2 (x    ) (y    )
          , V2 (x + w) (y    )
          , V2 (x + w) (y + h)
          , V2 (x    ) (y + h)]


-- | Rendering the ball.
renderBall :: Ball -> IO ()
renderBall _ = return ()

-- | Rendering a given world.
renderWorld :: World -> IO ()
renderWorld (World lp _ rp _ b) = do
  renderPaddle lp
  renderPaddle rp
  renderBall b
