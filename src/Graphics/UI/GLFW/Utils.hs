{-# OPTIONS -Wall #-}
module Graphics.UI.GLFW.Utils(GLFWEvent(..), withGLFW, eventLoop, openWindow) where

import Control.Concurrent(threadDelay)
import Control.Exception(bracket_)
import Control.Monad(forever, unless)
import Data.IORef(newIORef, atomicModifyIORef, modifyIORef)
import qualified Graphics.UI.GLFW as GLFW

-- This is the reification of the callback information:
data GLFWRawEvent = RawCharEvent Char Bool
                  | RawKeyEvent GLFW.Key Bool
                  | RawWindowClose
  deriving (Show, Eq)

-- This is the final representation we expose of events:
data GLFWEvent = KeyEvent (Maybe Char) GLFW.Key Bool
               | WindowClose
  deriving (Show, Eq)

isModifierKey :: GLFW.Key -> Bool
isModifierKey GLFW.KeyRightShift = True
isModifierKey GLFW.KeyLeftShift = True
isModifierKey GLFW.KeyRightCtrl = True
isModifierKey GLFW.KeyLeftCtrl = True
isModifierKey GLFW.KeyRightAlt = True
isModifierKey GLFW.KeyLeftAlt = True
isModifierKey _ = False

translate :: [GLFWRawEvent] -> [GLFWEvent]
translate [] = []
translate (RawWindowClose : xs) = WindowClose : translate xs
translate (rk@(RawKeyEvent key isPress1) : rc@(RawCharEvent char isPress2) : xs)
  | isModifierKey key = KeyEvent Nothing key isPress1 : translate xs
  | isPress1 == isPress2 =  KeyEvent (Just char) key isPress1 : translate xs
  | otherwise =  error $ "RawCharEvent " ++ show rc ++ " mismatches the RawKeyEvent: " ++ show rk
translate (RawKeyEvent key isPress : xs) = KeyEvent Nothing key isPress : translate xs
translate (RawCharEvent _ _ : xs) = translate xs -- This happens when you press shift while a key is pressed, ignore

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

withGLFW :: IO a -> IO a
withGLFW = bracket_ (GLFW.initialize >>= assert "initialize failed") GLFW.terminate

openWindow :: GLFW.DisplayOptions -> IO ()
openWindow options = GLFW.openWindow options >>= assert "Open window failed"

eventLoop :: ([GLFWEvent] -> IO ()) -> IO b
eventLoop iteration = do
  eventsVar <- newIORef []

  let
    addEvent = modifyIORef eventsVar . (:)
    addKeyEvent c k = addEvent $ RawKeyEvent c k
    charEventHandler c isPress
      | '\57344' <= c && c <= '\63743' = return () -- Range for "key" characters (keys for left key, right key, etc.)
      | otherwise = addEvent $ RawCharEvent c isPress

  GLFW.setCharCallback charEventHandler
  GLFW.setKeyCallback addKeyEvent
  GLFW.setWindowCloseCallback $ addEvent RawWindowClose >> return True

  forever $ do
    GLFW.pollEvents
    events <- atomicModifyIORef eventsVar (\o -> ([], translate (reverse o)))
    iteration events
    GLFW.swapBuffers
    threadDelay 10000
