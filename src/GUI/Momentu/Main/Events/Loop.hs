-- | Low-level event-loop

module GUI.Momentu.Main.Events.Loop
    ( Event(..)
    , Next(..)
    , EventLoopDisallowedWhenMasked(..)
    , eventLoop, wakeUp
    ) where

import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (when, void)
import           Data.Typeable (Typeable)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Main.Events
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           Prelude

-- | The output of the event handler back to the event-loop.
data Next
    = NextWait
    -- ^ idle wait for the next event
    | NextPoll
    -- ^ poll for the next event and immediately (up to sync-to-vblank) continue
    | NextQuit
    -- ^ The event-loop should quit.
    deriving (Show, Eq, Ord)

data EventLoopDisallowedWhenMasked = EventLoopDisallowedWhenMasked
    deriving (Show, Typeable)
instance E.Exception EventLoopDisallowedWhenMasked

mouseButtonEvent ::
    GLFW.Window -> (Event -> IO a) -> GLFW.MouseButton -> GLFW.MouseButtonState ->
    GLFW.ModifierKeys -> IO a
mouseButtonEvent win eventHandler button buttonState modKeys =
    do
        fbSize <- GLFW.Utils.framebufferSize win
        winSize <- GLFW.Utils.windowSize win
        p <- GLFW.getCursorPos win <&> uncurry Vector2
        EventMouseButton MouseButtonEvent
            { mbButton = button
            , mbButtonState = buttonState
            , mbModKeys = modKeys
            , mbPosition = p * fbSize / winSize
            , mbPositionInWindowCoords = p
            } & eventHandler

keyEvent :: (Event -> a) -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> a
keyEvent eventHandler key scanCode keyState modKeys =
    EventKey KeyEvent
    { keKey = key
    , keScanCode = scanCode
    , keState = keyState
    , keModKeys = modKeys
    } & eventHandler

framebufferSizeEvent :: (Event -> a) -> Int -> Int -> a
framebufferSizeEvent eventHandler w h =
    Vector2 w h & EventFramebufferSize & eventHandler

charEvent :: Monoid a => (Event -> a) -> Char -> a
charEvent eventHandler char
    -- Range for "key" characters (keys for left key, right key, etc.)
    | char < '\57344' || '\63743' < char = EventChar char & eventHandler
    | otherwise = mempty

validateMasksingState :: IO ()
validateMasksingState =
    do
        maskingState <- E.getMaskingState
        when (maskingState /= E.Unmasked) $ E.throwIO EventLoopDisallowedWhenMasked

-- | Exit current iteration, when it is waiting on an event
wakeUp :: IO ()
wakeUp = GLFW.postEmptyEvent

eventLoop :: GLFW.Window -> (Event -> IO Bool) -> IO Next -> IO ()
eventLoop win eventHandler iteration =
    do
        validateMasksingState
        let setCallback f cb = f win $ Just $ const cb
        setCallback GLFW.setCharCallback (charEvent veventHandler)
        setCallback GLFW.setKeyCallback (keyEvent veventHandler)
        setCallback GLFW.setMouseButtonCallback (mouseButtonEvent win veventHandler)
        setCallback GLFW.setDropCallback (veventHandler . EventDropPaths)
        setCallback GLFW.setWindowRefreshCallback $ veventHandler EventWindowRefresh
        setCallback GLFW.setFramebufferSizeCallback $ framebufferSizeEvent veventHandler
        setCallback GLFW.setWindowCloseCallback $ veventHandler EventWindowClose

        GLFW.swapInterval 1

        veventHandler EventWindowRefresh

        let loop =
                iteration
                >>= \case
                NextWait -> GLFW.waitEvents *> loop
                NextPoll -> GLFW.pollEvents *> loop
                NextQuit -> pure ()
        loop
    where
        veventHandler = void . eventHandler
