{-# OPTIONS -Wall #-}
module GLFWWrap(GLFWEvent(..), withGLFW, eventLoop, openWindow) where

import Control.Monad(forever, unless)
import Graphics.Rendering.OpenGL(($=), Size)
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef(newIORef, atomicModifyIORef, modifyIORef)
import Control.Exception(bracket_)
import Control.Concurrent(threadDelay)

data GLFWEvent = CharEvent Char GLFW.KeyButtonState
               | KeyEvent GLFW.Key GLFW.KeyButtonState
               | WindowClose
  deriving (Show, Eq)

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

openWindow :: Size
           -> [GLFW.DisplayBits]
           -> GLFW.WindowMode
           -> IO ()
openWindow size opts mode = GLFW.openWindow size opts mode >>=
                            assert "Open window failed"

withGLFW :: IO a -> IO a
withGLFW = bracket_ (GLFW.initialize >>= assert "initialize failed") GLFW.terminate

eventLoop :: ([GLFWEvent] -> IO ()) -> IO b
eventLoop iteration = do
  eventsVar <- newIORef []

  let addEvent = modifyIORef eventsVar . (:)
      addKeyEvent con c k = addEvent (con c k)

  GLFW.charCallback $= addKeyEvent CharEvent
  GLFW.keyCallback $= addKeyEvent KeyEvent
  GLFW.windowCloseCallback $= addEvent WindowClose

  forever $ do
    GLFW.pollEvents
    events <- atomicModifyIORef eventsVar (\o -> ([], reverse o))
    iteration events
    GLFW.swapBuffers
    threadDelay 10000
