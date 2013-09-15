module Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys) where

import qualified Graphics.UI.GLFW as GLFW

data DirKeys = DirKeys
  { keysLeft, keysRight, keysUp, keysDown :: [GLFW.Key]
  }

stdDirKeys :: DirKeys
stdDirKeys = DirKeys
  { keysLeft  = [GLFW.Key'Left,  GLFW.Key'H]
  , keysRight = [GLFW.Key'Right, GLFW.Key'L]
  , keysUp    = [GLFW.Key'Up,    GLFW.Key'K]
  , keysDown  = [GLFW.Key'Down,  GLFW.Key'J]
  }
