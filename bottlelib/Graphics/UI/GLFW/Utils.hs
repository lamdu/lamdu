module Graphics.UI.GLFW.Utils(withGLFW, createWindow, getVideoModeSize) where

import Control.Exception(bracket_)
import Control.Monad(unless)
import Control.MonadA (MonadA)
import Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW

assert :: MonadA m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

withGLFW :: IO a -> IO a
withGLFW = bracket_ (GLFW.init >>= assert "initialize failed") GLFW.terminate

createWindow :: Int -> Int -> String -> IO GLFW.Window
createWindow w h title = do
    mWin <- GLFW.createWindow w h title Nothing Nothing
    case mWin of
        Nothing -> fail "Open window failed"
        Just win -> do
            GLFW.makeContextCurrent $ Just win
            return win

getVideoModeSize :: IO (Vector2 Int)
getVideoModeSize = do
    monitor <-
        maybe (fail "GLFW: Can't get primary monitor") return =<<
        GLFW.getPrimaryMonitor
    videoMode <-
        maybe (fail "GLFW: Can't get video mode of monitor") return =<<
        GLFW.getVideoMode monitor
    return $ Vector2 (GLFW.videoModeWidth videoMode) (GLFW.videoModeHeight videoMode)
