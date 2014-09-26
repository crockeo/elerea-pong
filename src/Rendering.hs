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

-- | Generating a quad to be rendered given the position and size of a
--   rectangle.
generateQuad :: V2 Float -> V2 Float -> [V2 Float]
generateQuad (V2 x y) (V2 w h) =
  [ V2 (x    ) (y    )
  , V2 (x + w) (y    )
  , V2 (x + w) (y + h)
  , V2 (x    ) (y + h)
  ]

-- | Rendering a paddle.
renderPaddle :: Paddle -> IO ()
renderPaddle (Paddle p s) = do
  renderPrimitive Quads $
    mapM_ linearVertex $ generateQuad p s

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

-- | Rendering the score.
renderScore :: Int -> Either () () -> IO ()
renderScore s side = do
  (V2 w h) <- renderSize'
  renderPrimitive Quads $
    mapM_ linearVertex $ generateQuad (V2 (makeX w side) (-h + scoreHeight / 2)) (V2 (sigSide side * (fromIntegral s / 2)) scoreHeight)
  where makeX :: Float -> Either () () -> Float
        makeX w (Left  ()) = (-w + w / 4)
        makeX w (Right ()) = ( w - w / 4)

        sigSide :: Num a => Either () () -> a
        sigSide (Left  ()) =  1
        sigSide (Right ()) = -1

-- | Rendering a given world.
renderWorld :: World -> IO ()
renderWorld (World lp ls rp rs b) = do
  color $ Color3 (0.3 :: GLfloat) (0.3 :: GLfloat) (0.3 :: GLfloat)
  renderCenter

  color $ Color3 (1.0 :: GLfloat) (1.0 :: GLfloat) (1.0 :: GLfloat)
  renderPaddle lp
  renderPaddle rp
  renderBall b

  color $ Color3 (1.0 :: GLfloat) (1.0 :: GLfloat) (0.8 :: GLfloat)
  renderScore ls $ Left  ()
  renderScore rs $ Right ()
