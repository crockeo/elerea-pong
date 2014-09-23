module World where

--------------------
-- Global Imports --
import FRP.Elerea.Param

----------
-- Code --

-- | The data structure that represents the world.
data World = World

-- | Rendering the world to the screen.
renderWorld :: World -> IO ()
renderWorld _ = return ()

-- | The most recent version of the world.
world :: SignalGen Float (Signal World)
world = return $ return World
