module Network where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import FRP.Elerea.Param
import Data.IORef

-------------------
-- Local Imports --
import Rendering
import World
import Game

----------
-- Code --

-- | The backend for running the network.
runNetwork' :: IORef Bool -> (Float -> IO World) -> IO ()
runNetwork' closedRef stepFn = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      t <- get GLFW.time
      GLFW.time $= 0

      w <- stepFn $ realToFrac t

      clear [ColorBuffer]
      renderWorld w
      swapBuffers

      runNetwork' closedRef stepFn

-- | The front end for running the network.
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = do
  GLFW.time $= 0
  start worldWire >>= runNetwork' closedRef
