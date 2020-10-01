module Graphics.UI.GLFW.Utils
    ( withGLFW
    , printGLVersion
    , createWindow
    , getPrimaryMonitor
    , getVideoModeSize
    , getDisplayScale
    , charOfKey
    , windowSize
    , framebufferSize
    ) where

import           Control.Exception (bracket_)
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (unless)
import           Data.Foldable (traverse_)
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Stack (currentCallStack)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.IO (hPutStrLn, hFlush, stderr)

import           Prelude

assert :: MonadFail m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

printGLVersion :: IO ()
printGLVersion =
    do
        ver <- GL.get GL.glVersion
        putStrLn $ "Using GL version: " ++ show ver

printErrors :: GLFW.ErrorCallback
printErrors err msg =
    do
        put $ unwords ["GLFW error:", show err, msg]
        put "From: "
        currentCallStack >>= traverse_ (put . ("  "++))
        hFlush stderr
    where
        put = hPutStrLn stderr

withGLFW :: IO a -> IO a
withGLFW act =
    bracket_ (GLFW.init >>= assert "initialize failed") GLFW.terminate $
    do
        GLFW.setErrorCallback (Just printErrors)
        act

createWindow :: String -> Maybe GLFW.Monitor -> Vector2 Int -> IO GLFW.Window
createWindow title mMonitor (Vector2 w h) = do
    mWin <- GLFW.createWindow w h title mMonitor Nothing
    case mWin of
        Nothing -> fail "Open window failed"
        Just win -> win <$ GLFW.makeContextCurrent (Just win)

getVideoModeSize :: GLFW.Monitor -> IO (Vector2 Int)
getVideoModeSize monitor =
    GLFW.getVideoMode monitor
    >>= maybe (fail "GLFW: Can't get video mode of monitor") pure
    <&> \videoMode ->
    Vector2 (GLFW.videoModeWidth videoMode) (GLFW.videoModeHeight videoMode)

getPrimaryMonitor :: IO GLFW.Monitor
getPrimaryMonitor =
    GLFW.getPrimaryMonitor >>= maybe (fail "Cannot get primary monitor") pure

guessMonitor :: GLFW.Window -> IO GLFW.Monitor
guessMonitor window =
    GLFW.getWindowMonitor window
    >>= \case
    Just monitor -> pure monitor
    Nothing -> getPrimaryMonitor

stdPixelsPerInch :: Num a => Vector2 a
stdPixelsPerInch = Vector2 96 96

stdPixelsPerMM :: Fractional a => Vector2 a
stdPixelsPerMM = stdPixelsPerInch / 25.4

windowSize :: Num a => GLFW.Window -> IO (Vector2 a)
windowSize window =
    GLFW.getWindowSize window <&> uncurry Vector2 <&> fmap fromIntegral

framebufferSize :: Num a => GLFW.Window -> IO (Vector2 a)
framebufferSize window =
    GLFW.getFramebufferSize window <&> uncurry Vector2 <&> fmap fromIntegral

getDisplayScale :: Fractional a => GLFW.Window -> IO (Vector2 a)
getDisplayScale window =
    do
        monitor <- guessMonitor window
        unitsPerMM <-
            do
                monitorMM <- GLFW.getMonitorPhysicalSize monitor <&> uncurry Vector2
                monitorUnits <- getVideoModeSize monitor
                if monitorMM ^. _1 == 0 || monitorMM ^. _2 == 0
                    then pure stdPixelsPerMM
                    else (monitorUnits <&> fromIntegral) / (monitorMM <&> fromIntegral) & pure
        pixelsPerUnit <-
            do
                windowUnits <- windowSize window
                windowPixels <- framebufferSize window
                windowPixels / windowUnits & pure
        pixelsPerUnit * unitsPerMM / stdPixelsPerMM & pure

charOfKey :: GLFW.Key -> Maybe Char
charOfKey key =
    case key of
    GLFW.Key'A           -> Just 'A'
    GLFW.Key'B           -> Just 'B'
    GLFW.Key'C           -> Just 'C'
    GLFW.Key'D           -> Just 'D'
    GLFW.Key'E           -> Just 'E'
    GLFW.Key'F           -> Just 'F'
    GLFW.Key'G           -> Just 'G'
    GLFW.Key'H           -> Just 'H'
    GLFW.Key'I           -> Just 'I'
    GLFW.Key'J           -> Just 'J'
    GLFW.Key'K           -> Just 'K'
    GLFW.Key'L           -> Just 'L'
    GLFW.Key'M           -> Just 'M'
    GLFW.Key'N           -> Just 'N'
    GLFW.Key'O           -> Just 'O'
    GLFW.Key'P           -> Just 'P'
    GLFW.Key'Q           -> Just 'Q'
    GLFW.Key'R           -> Just 'R'
    GLFW.Key'S           -> Just 'S'
    GLFW.Key'T           -> Just 'T'
    GLFW.Key'U           -> Just 'U'
    GLFW.Key'V           -> Just 'V'
    GLFW.Key'W           -> Just 'W'
    GLFW.Key'X           -> Just 'X'
    GLFW.Key'Y           -> Just 'Y'
    GLFW.Key'Z           -> Just 'Z'
    GLFW.Key'Comma       -> Just ','
    GLFW.Key'Enter       -> Just '\n'
    GLFW.Key'Equal       -> Just '='
    GLFW.Key'GraveAccent -> Just '`'
    GLFW.Key'Minus       -> Just '-'
    GLFW.Key'Space       -> Just ' '
    GLFW.Key'Pad0        -> Just '0'
    GLFW.Key'Pad1        -> Just '1'
    GLFW.Key'Pad2        -> Just '2'
    GLFW.Key'Pad3        -> Just '3'
    GLFW.Key'Pad4        -> Just '4'
    GLFW.Key'Pad5        -> Just '5'
    GLFW.Key'Pad6        -> Just '6'
    GLFW.Key'Pad7        -> Just '7'
    GLFW.Key'Pad8        -> Just '8'
    GLFW.Key'Pad9        -> Just '9'
    GLFW.Key'PadDivide   -> Just '/'
    GLFW.Key'PadMultiply -> Just '*'
    GLFW.Key'PadSubtract -> Just '-'
    GLFW.Key'PadAdd      -> Just '+'
    GLFW.Key'PadDecimal  -> Just '.'
    GLFW.Key'PadEqual    -> Just '='
    _              -> Nothing
