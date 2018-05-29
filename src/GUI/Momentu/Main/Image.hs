-- | A main-loop that draws DrawingCombinator images and reacts to GLFW events,
-- given a Handlers record from the user.
--
-- The higher level GUI.Momentu.MainLoop builds upon this main-loop.

module GUI.Momentu.Main.Image
    ( mainLoop, Handlers(..)
    , PerfCounters(..)
    , windowSize
    ) where

import           Data.IORef
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (Size)
import qualified GUI.Momentu.Font as Font
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators.Extended as Draw
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event, Next(..), eventLoop)
import qualified Graphics.UI.GLFW.Events as GLFWEvents
import           System.TimeIt (timeItT)
import           Text.Printf (printf)

import           Lamdu.Prelude

data PerfCounters = PerfCounters
    { renderTime :: Double
    , swapBuffersTime :: Double
    }

data Handlers = Handlers
    { eventHandler :: Event -> IO ()
    , update :: IO (Maybe (Draw.Image ()))
    , refresh :: IO (Draw.Image ())
    , fpsFont :: IO (Maybe Draw.Font)
    , reportPerfCounters :: PerfCounters -> IO ()
    }

data EventResult =
    ERNone | ERRefresh | ERQuit
    deriving (Eq, Ord, Show)
instance Semigroup EventResult where
    (<>) = max
instance Monoid EventResult where
    mempty = ERNone
    mappend = (<>)

windowSize :: GLFW.Window -> IO Size
windowSize win =
    GLFW.getFramebufferSize win <&> uncurry Vector2 <&> fmap fromIntegral

newtype FPS = FPS (IORef UTCTime)

newFPS :: IO FPS
newFPS = getCurrentTime >>= newIORef <&> FPS

updateFPS :: FPS -> IO Double
updateFPS (FPS ref) =
    do
        curTime <- getCurrentTime
        prevTime <- readIORef ref
        writeIORef ref curTime
        1 / realToFrac (curTime `diffUTCTime` prevTime) & pure

renderFPS :: Draw.Font -> GLFW.Window -> Double -> IO (Draw.Image ())
renderFPS font win fps =
    do
        let fpsText = Text.pack (printf "%2.2f" fps)
        let Font.RenderedText sz img =
                Font.render font white Nothing fpsText
        winSize <- windowSize win
        let translation = winSize - (sz ^. Font.bounding)
        pure (Draw.translateV translation %% img)
    where
        white = Draw.Color 1 1 1 1

glDraw :: GLFW.Window -> Vector2 Double -> Draw.Image a -> IO PerfCounters
glDraw win (Vector2 winSizeX winSizeY) image =
    do
        GL.viewport $=
            (GL.Position 0 0,
             GL.Size (round winSizeX) (round winSizeY))
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho 0 winSizeX winSizeY 0 (-1) 1
        (timedRender, ()) <- timeItT (Draw.clearRender image)
        (timedSwapBuffers, ()) <- timeItT (GLFW.swapBuffers win)
        pure PerfCounters
            { renderTime = timedRender
            , swapBuffersTime = timedSwapBuffers
            }

mainLoop :: GLFW.Window -> (Size -> Handlers) -> IO ()
mainLoop win imageHandlers =
    do
        initialSize <- windowSize win
        frameBufferSize <- newIORef initialSize
        drawnImageHandlers <- imageHandlers initialSize & newIORef
        fps <- newFPS
        let handleEvent GLFWEvents.EventWindowClose = pure ERQuit
            handleEvent GLFWEvents.EventWindowRefresh = pure ERRefresh
            handleEvent (GLFWEvents.EventFrameBufferSize size) =
                ERRefresh <$ writeIORef frameBufferSize (fromIntegral <$> size)
            handleEvent event =
                do
                    handlers <- readIORef drawnImageHandlers
                    eventHandler handlers event
                    pure ERNone
        let handleEvents events =
                do
                    eventResult <- mconcat <$> traverse handleEvent events
                    winSize <- readIORef frameBufferSize
                    let handlers = imageHandlers winSize
                    writeIORef drawnImageHandlers handlers
                    fpsImg <-
                        fpsFont handlers >>= \case
                        Nothing -> pure mempty
                        Just font -> updateFPS fps >>= renderFPS font win
                    let draw img =
                            NextPoll <$
                            (glDraw win winSize (fpsImg <> img)
                                >>= reportPerfCounters handlers)
                    case eventResult of
                        ERQuit -> pure NextQuit
                        ERRefresh -> refresh handlers >>= draw
                        ERNone ->
                            update handlers >>=
                            maybe (pure NextWait) draw
        eventLoop win handleEvents
