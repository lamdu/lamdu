{-# OPTIONS -Wall #-}
module Graphics.UI.GLFWWidgets.KeyHandlers (modifiersEventHandlerWrap) where

import Control.Monad
import Data.IORef
import Data.Set(Set)
import Graphics.UI.GLFW
import Graphics.UI.GLFWWidgets.EventMap
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

modStateFromKeySet :: Set Key -> ModState
modStateFromKeySet keySet =
  ModState {
    modCtrl = isPressed [KeyLeftCtrl, KeyRightCtrl],
    modMeta = False, -- TODO: GLFW doesn't support meta/winkey?
    modAlt = isPressed [KeyLeftAlt, KeyRightAlt],
    modShift = isPressed [KeyLeftShift, KeyRightShift]
    }
  where
    isPressed = any (`Set.member` keySet)

modifiersEventHandlerWrap :: (Event -> IO ()) -> IO (GLFWUtils.GLFWEvent -> IO ())
modifiersEventHandlerWrap wrappedHandler = do
  keySetVar <- newIORef Set.empty
  let
    handler (GLFWUtils.KeyEvent key True) = do
      modifyIORef keySetVar (Set.insert key)
      keySet <- readIORef keySetVar
      wrappedHandler $ KeyEvent (modStateFromKeySet keySet) key
    handler (GLFWUtils.KeyEvent key False) =
      modifyIORef keySetVar (Set.delete key)
    handler (GLFWUtils.CharEvent char True) = do
      keySet <- readIORef keySetVar
      when (modStateFromKeySet keySet `elem` [noMods, shift]) . wrappedHandler $ CharEvent char
    handler _ = return ()
  return handler

