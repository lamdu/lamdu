-- | Draw the FPS on screen

module GUI.Momentu.Draw.FPS
    ( FPS
    , new
    , update
    , render
    ) where

import           Data.IORef
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators.Extended as Draw
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils
import           Text.Printf (printf)

import           GUI.Momentu.Prelude

newtype FPS = FPS (IORef UTCTime)

new :: IO FPS
new = getCurrentTime >>= newIORef <&> FPS

update :: FPS -> IO Double
update (FPS ref) =
    do
        curTime <- getCurrentTime
        prevTime <- readIORef ref
        writeIORef ref curTime
        1 / realToFrac (curTime `diffUTCTime` prevTime) & pure

render :: Font -> GLFW.Window -> Double -> IO (Draw.Image ())
render font win fps =
    do
        let fpsText = Text.pack (printf "%2.2f" fps)
        let Font.RenderedText sz img =
                Font.render font white Nothing fpsText
        winSize <- GLFW.Utils.framebufferSize win
        let translation = winSize - sz ^. Font.bounding
        pure (Draw.translateV translation %% img)
    where
        white = Draw.Color 1 1 1 1
