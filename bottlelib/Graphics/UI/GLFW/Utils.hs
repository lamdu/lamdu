{-# OPTIONS -Wall #-}
module Graphics.UI.GLFW.Utils(withGLFW, openWindow) where

import Control.Exception(bracket_)
import Control.Monad(unless)
import qualified Graphics.UI.GLFW as GLFW

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

withGLFW :: IO a -> IO a
withGLFW = bracket_ (GLFW.initialize >>= assert "initialize failed") GLFW.terminate

openWindow :: GLFW.DisplayOptions -> IO ()
openWindow options = GLFW.openWindow options >>= assert "Open window failed"
