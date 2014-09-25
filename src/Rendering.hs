module Rendering (renderWorld) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Linear.V2

-------------------
-- Local Imports --
import Config
import Input
import World

----------
-- Code --

-- | Binding a @'V2' 'Float'@ to a @'vertex'@ command.
linearVertex :: V2 Float -> IO ()
linearVertex (V2 x y) =
  vertex $ Vertex2 (realToFrac x :: GLfloat)
                   (realToFrac y :: GLfloat)

-- | Rendering a paddle.
renderPaddle :: Paddle -> IO ()
renderPaddle (Paddle (V2 x y) (V2 w h)) = do
  renderPrimitive Quads $
    mapM_ linearVertex [ V2 (x    ) (y    )
                       , V2 (x + w) (y    )
                       , V2 (x + w) (y + h)
                       , V2 (x    ) (y + h)
                       ]

-- | Rendering a line down the center of the screen.
renderCenter :: IO ()
renderCenter = do
  (V2 _ h) <- renderSize'
  renderPrimitive Lines $
    mapM_ linearVertex [ V2 0 (-h)
                       , V2 0 ( h)
                       ]

-- | Rendering the ball.
renderBall :: Ball -> IO ()
renderBall (Ball pos r) =
  renderPrimitive TriangleFan $
    mapM_ linearVertex $ pos : gvs 0
  where gvs :: Float -> [V2 Float]
        gvs radians
          | radians > 360 = []
          | otherwise     = generateVertex : gvs (radians + (2 * pi / renderDetail))
          where generateVertex :: V2 Float
                generateVertex = pos + (V2 r r) * V2 (sin radians) (cos radians)

-- | Rendering a given world.
renderWorld :: World -> IO ()
renderWorld (World lp _ rp _ b) = do
  renderCenter
  renderPaddle lp
  renderPaddle rp
  renderBall b
