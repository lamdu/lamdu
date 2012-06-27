module Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys) where

import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.GLFW as GLFW

data DirKeys = DirKeys
  { keysLeft, keysRight, keysUp, keysDown :: [GLFW.Key]
  }

stdDirKeys :: DirKeys
stdDirKeys = DirKeys
  { keysLeft  = [GLFW.KeyLeft,  EventMap.charKey 'h']
  , keysRight = [GLFW.KeyRight, EventMap.charKey 'l']
  , keysUp    = [GLFW.KeyUp,    EventMap.charKey 'k']
  , keysDown  = [GLFW.KeyDown,  EventMap.charKey 'j']
  }
