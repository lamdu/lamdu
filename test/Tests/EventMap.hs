module Tests.EventMap where

import           Control.Lens (runIdentity)
import           GUI.Momentu.EventMap hiding (KeyEvent)
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event(..), KeyEvent(..))

import           Test.Lamdu.Prelude hiding (lookup)

charEvent :: Char -> GLFW.Key -> Event
charEvent c key = EventKey KeyEvent
    { keKey = key
    , keScanCode = 0 -- dummy
    , keModKeys = mempty
    , keState = GLFW.KeyState'Pressed
    , keChar = Just c
    }

eventMap :: EventMap Integer
eventMap =
    mconcat
    [ charEventMap "A" (Doc ["B"]) (const Nothing)
    , charEventMap "C" (Doc ["D"]) (const (Just 1))
    ]

test :: Test
test =
    assertEqual "Lookup" (Just (Doc ["D"])) (res <&> (^. dhDoc))
    & testCase "event-map-lookup for char handlers"
    where
        res =
            lookup (pure Nothing) (charEvent 'X' GLFW.Key'X) eventMap
            & runIdentity
