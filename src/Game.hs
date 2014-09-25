module Game where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param

-------------------
-- Local Imports --
import Paddle
import World
import Ball

----------
-- Code --

-- | The wire to represent the world.
worldWire :: SignalGen Float (Signal World)
worldWire = do
  lp <- paddleWire (CharKey 'W') (CharKey 'S') (Left  ())
  rp <- paddleWire (UP         ) (DOWN       ) (Right ())
  b  <- ballWire

  return $ World <$> lp <*> pure 0 <*> rp <*> pure 0 <*> b
