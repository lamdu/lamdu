{-# LANGUAGE LambdaCase #-}
module Graphics.UI.GLFW.Utils
    ( withGLFW
    , createWindow
    , getVideoModeSize
    , getDisplayScale
    ) where

import           Control.Exception (bracket_)
import           Control.Lens.Operators
import           Control.Monad (unless)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

printErrors :: GLFW.ErrorCallback
printErrors err msg = putStrLn $ unwords ["GLFW error:", show err, msg]

withGLFW :: IO a -> IO a
withGLFW act =
    do
        GLFW.setErrorCallback (Just printErrors)
        bracket_ (GLFW.init >>= assert "initialize failed") GLFW.terminate act

createWindow :: String -> Maybe GLFW.Monitor -> Vector2 Int -> IO GLFW.Window
createWindow title mMonitor (Vector2 w h) = do
    mWin <- GLFW.createWindow w h title mMonitor Nothing
    case mWin of
        Nothing -> fail "Open window failed"
        Just win -> do
            GLFW.makeContextCurrent $ Just win
            return win

getVideoModeSize :: GLFW.Monitor -> IO (Vector2 Int)
getVideoModeSize monitor = do
    videoMode <-
        maybe (fail "GLFW: Can't get video mode of monitor") return =<<
        GLFW.getVideoMode monitor
    return $ Vector2 (GLFW.videoModeWidth videoMode) (GLFW.videoModeHeight videoMode)

guessMonitor :: GLFW.Window -> IO GLFW.Monitor
guessMonitor window =
    GLFW.getWindowMonitor window
    >>= \case
    Just monitor -> return monitor
    Nothing ->
        GLFW.getPrimaryMonitor
        >>= maybe (fail "Cannot get primary monitor") return

stdPixelsPerInch :: Num a => Vector2 a
stdPixelsPerInch = Vector2 96 96

stdPixelsPerMM :: Fractional a => Vector2 a
stdPixelsPerMM = stdPixelsPerInch / 25.4

getDisplayScale :: Fractional a => GLFW.Window -> IO (Vector2 a)
getDisplayScale window =
    do
        monitor <- guessMonitor window
        (widthMM, heightMM) <- GLFW.getMonitorPhysicalSize monitor
        let physSizeMM = Vector2 widthMM heightMM <&> fromIntegral
        videoModeSize <- getVideoModeSize monitor <&> fmap fromIntegral
        winSize <- GLFW.getWindowSize window <&> uncurry Vector2 <&> fmap fromIntegral
        let actualPixelsPerMM = videoModeSize / physSizeMM
        let physScale = actualPixelsPerMM / stdPixelsPerMM

        -- TODO: Is videoModeSize in "logical" or "framebuffer" pixels?
        -- If the latter, need to delete "winScale"
        fbSize <- GLFW.getFramebufferSize window <&> uncurry Vector2 <&> fmap fromIntegral
        let winScale = fbSize / winSize

        physScale * winScale & return
