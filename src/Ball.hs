module Ball where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Linear.V2

-------------------
-- Local Imports --
import Config
import World

----------
-- Code --

-- | Getting the speed of the ball.
ballSpeed :: Signal (V2 Bool) -> Signal (V2 Float) -> SignalGen Float (Signal (V2 Float))
ballSpeed b dir = do
  delay ballSpeedRaw $ ballSpeed' <$> b <*> dir
  where ballSpeed' :: V2 Bool -> V2 Float -> V2 Float
        ballSpeed' (V2 bx by) (V2 dx dy)
          | bx && by  = V2 (-dx * ballScale) (-dy * ballScale)
          | bx        = V2 (-dx * ballScale) ( dy)
          | by        = V2 ( dx) (-dy * ballScale)
          | otherwise = V2 ( dx) ( dy)

-- | Getting the position of the ball.
ballPosition :: Signal (V2 Bool, V2 Float) -> SignalGen Float (Signal (V2 Float))
ballPosition sBounds = do
  sSpeed <- mfix $ (ballSpeed $ fmap fst sBounds)
  sPos   <- transfer2 (V2 0 0) ballPosition' sSpeed sBounds

  delay (V2 0 0) sPos
  where ballPosition' :: Float -> V2 Float -> (V2 Bool, V2 Float) -> V2 Float -> V2 Float
        ballPosition' dt (V2 dx dy) (V2 bx by, V2 nx ny) (V2 x y)
          | bx && by  = V2 nx            ny
          | bx        = V2 nx            (y + dt * dy)
          | by        = V2 (x + dt * dx) ny
          | otherwise = V2 (x + dt * dx) (y + dt * dy)

-- | The wire to represent the ball.
ballWire :: Signal (V2 Bool, V2 Float) -> SignalGen Float (Signal Ball)
ballWire sBounds = do
  pos <- ballPosition sBounds
  return $ Ball <$> pos <*> pure ballRadius
