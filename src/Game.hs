module Game where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Monad.Fix
import FRP.Elerea.Param
import Linear.V2
import Debug.Trace

-------------------
-- Local Imports --
import Config
import Input
import Utils
import World

----------
-- Code --

-- | The initial paddle.
initialPaddle :: Either () () -> Paddle
initialPaddle (Left  ()) = Paddle (V2 (-50               + paddleWidth / 2) 0) paddleSize
initialPaddle (Right ()) = Paddle (V2 ( 50 - paddleWidth - paddleWidth / 2) 0) paddleSize

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
paddleVelocity :: Enum k => k -> k -> Signal Bool -> Signal Float -> SignalGen Float (Signal Float)
paddleVelocity uk dk sb vel = do
  accel <- paddleAcceleration uk dk vel
  vel'  <- transfer2 0 paddleVelocity' accel sb
  delay 0 vel'
  where paddleVelocity' :: Float -> Float -> Bool -> Float -> Float
        paddleVelocity' dt accel b vel' =
          let vel'' = (vel' + accel * dt) in
            if vel'' > -minSpeed && vel'' < minSpeed
              then 0
              else if b
                then -vel'' / 1.5
                else  vel''

-- | Checking if a paddle should bounce.
bounce :: Signal Float -> SignalGen Float (Signal Bool)
bounce sy = do
  h <- signalMap (\(V2 _ h) -> h) renderSize
  return $ bounce' <$> sy <*> h
  where bounce' :: Float -> Float -> Bool
        bounce' y h = y < -h || y + paddleHeight > h

-- | Getting the position of the paddle.
paddlePosition :: Enum k => k -> k -> Signal Float -> SignalGen Float (Signal Float)
paddlePosition uk dk p = do
  h   <- signalMap (\(V2 _ h) -> h) renderSize
  b   <- bounce p
  vel <- mfix $ paddleVelocity uk dk b
  p'  <- transfer3 (-paddleHeight / 2) paddlePosition' vel b h

  delay (-paddleHeight / 2) p'
  where paddlePosition' :: Float -> Float -> Bool -> Float -> Float -> Float
        paddlePosition' dt vel b h p' =
          if b
            then if p' > 0
              then  h - paddleHeight
              else -h
            else p' + vel * dt

-- | The wire to represent the paddle.
paddleWire :: Enum k => k -> k -> Either () () -> SignalGen Float (Signal Paddle)
paddleWire uk dk side = do
  w <- signalMap (\(V2 w _) -> w) renderSize
  y <- mfix $ paddlePosition uk dk

  return $ makePaddle <$> y <*> w
  where makePaddle :: Float -> Float -> Paddle
        makePaddle y w =
          case side of
            Left  () -> Paddle (V2 (-w               + paddleWidth / 2) y) paddleSize
            Right () -> Paddle (V2 ( w - paddleWidth - paddleWidth / 2) y) paddleSize

-- | Getting the speed of the ball.
ballSpeed :: Signal (Bool, Bool) -> Signal (V2 Float) -> SignalGen Float (Signal (V2 Float))
ballSpeed b dir = do
  delay ballSpeedRaw $ (fmap (\s -> traceShow s s) $ ballSpeed' <$> b <*> dir)
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

-- | The wire to represent the world.
worldWire :: SignalGen Float (Signal World)
worldWire = do
  lp <- paddleWire (CharKey 'W') (CharKey 'S') (Left  ())
  rp <- paddleWire (UP         ) (DOWN       ) (Right ())
  b  <- ballWire

  return $ World <$> lp <*> pure 0 <*> rp <*> pure 0 <*> b
