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
import Input
import World

----------
-- Code --

-- | Getting the speed of the ball.
ballSpeed :: Signal (Bool, Bool) -> Signal (V2 Float) -> SignalGen Float (Signal (V2 Float))
ballSpeed b dir = do
  delay ballSpeedRaw $ ballSpeed' <$> b <*> dir
  where ballSpeed' :: (Bool, Bool) -> V2 Float -> V2 Float
        ballSpeed' (bx, by) (V2 dx dy)
          | bx && by  = V2 (-dx) (-dy)
          | bx        = V2 (-dx) ( dy)
          | by        = V2 ( dx) (-dy)
          | otherwise = V2 ( dx) ( dy)

-- | Checking if a ball at a given position should bounce.
shouldBounce :: Signal (V2 Float) -> SignalGen Float (Signal (Bool, Bool))
shouldBounce p = do
  size <- renderSize
  return $ shouldBounce' <$> size <*> p
  where shouldBounce' :: V2 Float -> V2 Float -> (Bool, Bool)
        shouldBounce' (V2 w h) (V2 x y) =
          ( x - ballRadius < (-w) || x + ballRadius > ( w)
          , y - ballRadius < (-h) || y + ballRadius > ( h)
          )

-- | Getting the position of the ball.
ballPosition :: Signal (V2 Float) -> SignalGen Float (Signal (V2 Float))
ballPosition p = do
  size <- renderSize
  b    <- shouldBounce p
  s    <- mfix $ ballSpeed b
  p'   <- transfer3 (V2 0 0) ballPosition' size s b

  delay (V2 0 0) p'
  where ballPosition' :: Float -> V2 Float -> V2 Float -> (Bool, Bool) -> V2 Float -> V2 Float
        ballPosition' dt (V2 w h) speed (bx, by) p' =
          let p''@(V2 x y) = p' + speed * pure dt in
            if by
              then if y < 0
                then V2 x (-h + ballRadius)
                else V2 x ( h - ballRadius)
              else if bx
                then if x < 0
                  then V2 (-w + ballRadius) y
                  else V2 ( w - ballRadius) y
                else p''

-- | The wire to represent the ball.
ballWire :: SignalGen Float (Signal Ball)
ballWire = do
  pos <- mfix ballPosition
  return $ Ball <$> pos <*> pure ballRadius
