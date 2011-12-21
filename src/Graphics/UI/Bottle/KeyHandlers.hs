{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.KeyHandlers (modifiersEventHandlerWrap) where

import Data.IORef(newIORef, readIORef, modifyIORef)
import Data.Set(Set)
import Graphics.UI.GLFW(
  Key(KeyLeftAlt, KeyLeftCtrl, KeyLeftShift, KeyRightAlt, KeyRightCtrl, KeyRightShift))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

modStateFromKeySet :: Set Key -> E.ModState
modStateFromKeySet keySet =
  E.ModState {
    E.modCtrl = isPressed [KeyLeftCtrl, KeyRightCtrl],
    E.modMeta = False, -- TODO: GLFW doesn't support meta/winkey?
    E.modAlt = isPressed [KeyLeftAlt, KeyRightAlt],
    E.modShift = isPressed [KeyLeftShift, KeyRightShift]
    }
  where
    isPressed = any (`Set.member` keySet)

modifiersEventHandlerWrap :: (E.Event -> IO ()) -> IO (GLFWUtils.GLFWEvent -> IO ())
modifiersEventHandlerWrap wrappedHandler = do
  keySetVar <- newIORef Set.empty
  let
    handler (GLFWUtils.KeyEvent mchar key True) = do
      modifyIORef keySetVar (Set.insert key)
      keySet <- readIORef keySetVar
      wrappedHandler $ E.KeyEvent (modStateFromKeySet keySet) mchar key
    handler (GLFWUtils.KeyEvent _ key False) =
      modifyIORef keySetVar (Set.delete key)
    handler _ = return ()
  return handler

