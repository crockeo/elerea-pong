module Input where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Applicative
import FRP.Elerea.Param
import Linear.V2

----------
-- Code --

-- | Getting the current render size of the window in the IO monad.
renderSize' :: IO (V2 Float)
renderSize' =
  liftA convert $ get windowSize
  where convert :: Size -> V2 Float
        convert (Size w h) =
          V2 (fromIntegral w / 640 * 50)
             (fromIntegral h / 640 * 50)

-- | Getting the current render size of the window.
renderSize :: SignalGen p (Signal (V2 Float))
renderSize = effectful renderSize'

-- | Checking if a given key is held down.
isKeyDown :: Enum k => k -> SignalGen p (Signal Bool)
isKeyDown = effectful . liftA (== Press) . getKey

