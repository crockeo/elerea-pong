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
import Paddle
import Input
import World
import Ball

----------
-- Code --

-- | Tracking the score of the game.
trackScore :: Signal Bool -> SignalGen p (Signal Int)
trackScore sScore = do
  sS <- transfer 0 (\_ -> incIf) sScore
  delay 0 sS
  where incIf :: Bool -> Int -> Int
        incIf inc =
          if inc
            then succ
            else id

-- | Checking if a ball should bounce off the edge.
ballBounce :: Signal Ball -> Signal Paddle -> Signal Paddle -> SignalGen p (Signal (V2 Bool, V2 Float))
ballBounce sBall sLPaddle sRPaddle = do
  sSize <- renderSize
  return $ ballBounce' <$> sSize <*> sBall <*> sLPaddle <*> sRPaddle
  where ballBounce' :: V2 Float -> Ball -> Paddle -> Paddle -> (V2 Bool, V2 Float)
        ballBounce' (V2 w h)
                    b@(Ball (V2 bx by) br)
                    lp@(Paddle (V2 lpx _) (V2 lpw _))
                    rp@(Paddle (V2 rpx _) (V2   _ _))
          | bx - br < -w  = (V2  True False, V2 (-w + br) by       )
          | bx + br >  w  = (V2  True False, V2 ( w - br) by       )
          | by - br < -h  = (V2 False  True, V2 bx        (-h + br))
          | by + br >  h  = (V2 False  True, V2 bx        ( h - br))
          | collides b lp = (V2  True False, V2 (lpx + lpw + br) by)
          | collides b rp = (V2  True False, V2 (rpx       - br) by)
          | otherwise     = (V2 False False, V2 bx        by       )

        collides :: Ball -> Paddle -> Bool
        collides (Ball (V2 bx by) br) (Paddle (V2 px py) (V2 pw ph)) =
          bx + br > px && bx - br < px + pw &&
          by + br > py && by - br < py + ph

-- | Checking if the ball should register a score.
ballScore :: Signal Ball -> SignalGen p (Signal (V2 Bool))
ballScore sBall = do
  sSize <- renderSize
  delay (pure False) $ ballScore' <$> sBall <*> sSize
  where ballScore' :: Ball -> V2 Float -> V2 Bool
        ballScore' (Ball (V2 bx _) br) (V2 w _) =
          V2 (bx - br < -w)
             (bx + br >  w)

-- | The backend to the @'worldWire'@.
worldWire' :: Signal World -> SignalGen Float (Signal World)
worldWire' sWorld = do
  bBounce <- ballBounce (fmap ball sWorld) (fmap lPaddle sWorld) (fmap rPaddle sWorld)
  ss      <- ballScore (fmap ball sWorld)
  lp      <- paddleWire (CharKey 'W') (CharKey 'S') (Left  ())
  ls      <- trackScore $ fmap (\(V2 l _) -> l) ss
  rp      <- paddleWire (UP         ) (DOWN       ) (Right ())
  rs      <- trackScore $ fmap (\(V2 _ r) -> r) ss
  b       <- ballWire bBounce

  return $ World <$> lp <*> ls <*> rp <*> rs <*> b

-- | The wire to represent the world.
worldWire :: SignalGen Float (Signal World)
worldWire = mfix worldWire'

{-

-- | The wire to represent the world.
worldWire :: SignalGen Float (Signal World)
worldWire = do
  lp <- paddleWire (CharKey 'W') (CharKey 'S') (Left  ())
  rp <- paddleWire (UP         ) (DOWN       ) (Right ())
  b  <- ballWire

  return $ World <$> lp <*> pure 0 <*> rp <*> pure 0 <*> b

-}
