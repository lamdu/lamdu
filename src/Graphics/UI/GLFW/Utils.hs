{-# LANGUAGE LambdaCase #-}
module Graphics.UI.GLFW.Utils
    ( withGLFW
    , createWindow
    , getVideoModeSize
    , getDisplayScale
    , charOfKey
    ) where

import           Control.Concurrent (myThreadId)
import           Control.Exception (bracket_)
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (unless)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.GLFW as GLFW
import           System.IO (hFlush, stderr, hPutStrLn)

import           Prelude hiding (log)

log :: String -> IO ()
log msg =
    do
        tid <- myThreadId
        hPutStrLn stderr (show tid ++ ": " ++ msg)
        hFlush stderr

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

printErrors :: GLFW.ErrorCallback
printErrors err msg = log $ unwords ["GLFW error:", show err, msg]

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
        Just win -> win <$ GLFW.makeContextCurrent (Just win)

getVideoModeSize :: GLFW.Monitor -> IO (Vector2 Int)
getVideoModeSize monitor =
    GLFW.getVideoMode monitor
    >>= maybe (fail "GLFW: Can't get video mode of monitor") pure
    <&> \videoMode ->
    Vector2 (GLFW.videoModeWidth videoMode) (GLFW.videoModeHeight videoMode)

guessMonitor :: GLFW.Window -> IO GLFW.Monitor
guessMonitor window =
    GLFW.getWindowMonitor window
    >>= \case
    Just monitor -> pure monitor
    Nothing ->
        GLFW.getPrimaryMonitor
        >>= maybe (fail "Cannot get primary monitor") pure

stdPixelsPerInch :: Num a => Vector2 a
stdPixelsPerInch = Vector2 96 96

stdPixelsPerMM :: Fractional a => Vector2 a
stdPixelsPerMM = stdPixelsPerInch / 25.4

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
                windowUnits <- GLFW.getWindowSize window <&> uncurry Vector2 <&> fmap fromIntegral
                windowPixels <- GLFW.getFramebufferSize window <&> uncurry Vector2 <&> fmap fromIntegral
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
