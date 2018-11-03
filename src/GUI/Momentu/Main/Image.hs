-- | A main-loop that draws DrawingCombinator images and reacts to GLFW events,
-- given a Handlers record from the user.
--
-- The higher level GUI.Momentu.MainLoop builds upon this main-loop.

module GUI.Momentu.Main.Image
    ( mainLoop
    , PerfCounters(..)
    , TickResult(..)
    , Handlers(..)
    , EventLoop.wakeUp
    ) where

import           Data.IORef
import qualified GUI.Momentu.Draw.FPS as FPS
import           GUI.Momentu.Font (Font)
import           GUI.Momentu.Main.Events.Loop (Event, Next(..), eventLoop)
import qualified GUI.Momentu.Main.Events.Loop as EventLoop
import           GUI.Momentu.Render (PerfCounters(..), render)
import qualified Graphics.DrawingCombinators.Extended as Draw
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

data TickResult
    = StopTicking               -- | Stop ticking until we have more events
    | TickImage (Draw.Image ()) -- | Here's the next image, tick again please

data Handlers = Handlers
    { eventHandler :: Event -> IO Bool
        -- ^ Handle a single event, return whether event was handled or ignored
    , tick :: IO TickResult
        -- ^ A tick passed since we had an image update, yield the next image or stop
    , refresh :: IO (Draw.Image ())
        -- ^ Window redraw requested, please provide an image to draw
    , fpsFont :: IO (Maybe Font)
    , reportPerfCounters :: PerfCounters -> IO ()
    }

data EventResult = ERNone | ERRefresh | ERQuit deriving (Eq, Ord, Show)
instance Semigroup EventResult where
    (<>) = max
instance Monoid EventResult where
    mempty = ERNone
    mappend = (<>)

mainLoop :: GLFW.Window -> Handlers -> IO ()
mainLoop win handlers =
    do
        fps <- FPS.new
        eventResultRef <- newIORef mempty
        let iteration =
                do
                    fpsImg <-
                        fpsFont handlers >>= \case
                        Nothing -> pure mempty
                        Just font -> FPS.update fps >>= FPS.render font win
                    let draw img =
                            NextPoll <$
                            (render win (fpsImg <> img)
                                >>= reportPerfCounters handlers)
                    eventResult <- readIORef eventResultRef
                    writeIORef eventResultRef mempty
                    case eventResult of
                        ERQuit -> pure NextQuit
                        ERRefresh -> refresh handlers >>= draw
                        ERNone ->
                            tick handlers >>=
                            \case
                            StopTicking -> pure NextWait
                            TickImage image -> draw image
        let eventRes = modifyIORef eventResultRef . (<>)
        let handleEvent =
                \case
                EventLoop.EventWindowClose -> True <$ eventRes ERQuit
                EventLoop.EventWindowRefresh -> True <$ eventRes ERRefresh
                EventLoop.EventFramebufferSize {} -> True <$ eventRes ERRefresh
                event -> eventHandler handlers event
        eventLoop win handleEvent iteration
