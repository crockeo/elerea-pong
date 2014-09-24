module Game where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
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

-- | The initial paddle.
initialPaddle :: Either () () -> Paddle
initialPaddle (Left  ()) = Paddle (V2 (-50               + paddleWidth / 2) 0) paddleSize
initialPaddle (Right ()) = Paddle (V2 ( 50 - paddleWidth - paddleWidth / 2) 0) paddleSize

-- | Taking the integral of a value with respect to time.
integral :: Fractional a => a -> (Signal a -> SignalGen a (Signal a))
integral i =
  transfer i integral'
  where integral' :: Fractional a => a -> a -> a -> a
        integral' dt s p = p + s * dt

-- | Mapping a function over a @'SignalGen'@ of a @'Signal'@.
signalMap :: (a -> b) -> SignalGen p (Signal a) -> SignalGen p (Signal b)
signalMap fn s = fmap (fmap fn) s

-- | Generating the vertical acceleration given the input keys.
paddleAcceleration :: Enum k => k -> k -> Signal Float -> SignalGen Float (Signal Float)
paddleAcceleration uk dk v = do
  ud <- isKeyDown uk
  dd <- isKeyDown dk

  return $ acceleration <$> ud <*> dd <*> v
  where acceleration :: Bool -> Bool -> Float -> Float
        acceleration  True  True vel = decel vel
        acceleration  True False _   = (-paddleSpeed)
        acceleration False  True _   = ( paddleSpeed)
        acceleration False False vel = decel vel

        decel :: Float -> Float
        decel vel
          | vel > 0   = -paddleSpeed
          | vel < 0   =  paddleSpeed
          | otherwise = 0

-- | Generating the vertical velocity of a paddle.
paddleVelocity :: Enum k => k -> k -> Signal Float -> SignalGen Float (Signal Float)
paddleVelocity uk dk vel = do
  accel <- paddleAcceleration uk dk vel
  vel'  <- integral 0 accel

  delay 0 $ stop <$> vel'
  where stop :: Float -> Float
        stop f =
          if f > -minSpeed && f < minSpeed
            then 0
            else f

-- | The wire to represent the paddle.
paddleWire :: Enum k => k -> k -> Either () () -> SignalGen Float (Signal Paddle)
paddleWire uk dk side = do
  veloc <- mfix (paddleVelocity uk dk)
  w     <- signalMap (\(V2 w _) -> w) renderSize
  y     <- transfer (-paddleHeight / 2) paddlePosition veloc

  return $ makePaddle <$> y <*> w
  where paddlePosition :: Float -> Float -> Float -> Float
        paddlePosition dt accel pos = pos + accel * dt

        makePaddle :: Float -> Float -> Paddle
        makePaddle y w =
          case side of
            Left  () -> Paddle (V2 (-w               + paddleWidth / 2) y) paddleSize
            Right () -> Paddle (V2 ( w - paddleWidth - paddleWidth / 2) y) paddleSize


-- | The wire to represent the ball.
ballWire :: SignalGen Float (Signal Ball)
ballWire = pure $ pure $ Ball (V2 0 0) 5

-- | The wire to represent the world.
worldWire :: SignalGen Float (Signal World)
worldWire = do
  lp <- paddleWire (CharKey 'W') (CharKey 'S') (Left  ())
  rp <- paddleWire (UP         ) (DOWN       ) (Right ())
  b  <- ballWire

  return $ World <$> lp <*> pure 0 <*> rp <*> pure 0 <*> b
