module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
import Network

----------
-- Code --

-- | Creating a callback for closing the window.
makeWindowCloseCallback :: IORef Bool -> WindowCloseCallback
makeWindowCloseCallback closedRef = do
  writeIORef closedRef True
  return True

-- | Creating a callback for resizing the window.
makeWindowSizeCallback :: WindowSizeCallback
makeWindowSizeCallback s@(Size w h) = do
  matrixMode $= Projection
  loadIdentity

  ortho (-fromIntegral w / 640 * 50) ( fromIntegral w / 640 * 50)
        ( fromIntegral h / 640 * 50) (-fromIntegral h / 640 * 50)
        (             -1           ) (              1           )

  matrixMode $= Modelview 0

  viewport $= (Position 0 0, s)

-- | The entry point of the program.
main :: IO ()
main = do
  initialize
  openWindow (Size 640 480)
             [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24]
             Window
  windowTitle $= "elerea-pong"

  closedRef <- newIORef False

  windowCloseCallback $= makeWindowCloseCallback closedRef
  windowSizeCallback  $= makeWindowSizeCallback

  runNetwork closedRef
  closeWindow
