{-# LANGUAGE PatternSynonyms #-}
-- | A convenience API module that re-exports the functionality in the Momentu library

module GUI.Momentu
    (
    -- | Shortcut Keys
      MetaKey.ModifierKeys(..), MetaKey.noMods, MetaKey.cmd, MetaKey.shift
    , MetaKey(..), parseMetaKey, formatMetaKey
    , MetaKey.Key(..)

    -- | Events
    , EventMap.Doc(..)
    , Widget.weakerEvents

    -- | Animations
    , AnimId

    -- | Element class
    , Element(..), Element.tint, Element.width, Element.height, Element.padAround

    -- | Widget
    , Widget
    , Widget.isFocused
    , WidgetId, pattern WidgetId
    , State.Update
    , State.Gui

    -- | View
    , View
    , View.unitSquare

    -- | GUI Layout
    , (/-/), (/|/), Glued
    , above -- ^ A named alias for `/-/`
    , leftOf -- ^ A named alias for `/|/`

    -- | Widget alignment
    , Align.Aligned(..), Align.alignmentRatio, Align.value
    , Align.WithTextPos(..), Align.TextWidget, Align.textTop, Align.tValue

    -- | Drawing
    , Font.Font, Font.openFont
    , MDraw.Color(..)
    , MDraw.backgroundColor
    , MDraw.addInnerFrame

    -- | Environments
    , Element.HasAnimIdPrefix(..)
    , State.HasCursor(..)
    , State.GUIState(..)
    , State.HasState(..), State.readWidgetState
    , Widget.HasWidget(..)
    , Spacer.HasStdSpacing(..)

    -- | Setup
    , GLFWUtils.getPrimaryMonitor
    , GLFWUtils.getVideoModeSize
    , GLFWUtils.withGLFW

    , WindowMode(..)
    , createWindow, GLFW.Window

    -- | Main loop
    , Zoom, Zoom.getZoomFactor
    , MainLoopEnv, MainLoop.eZoom, MainLoop.eWindowSize, MainLoop.quitEventMap
    , MainLoop.mainLoopWidget, MainLoop.defaultOptions

    -- | Basic types
    , Vector2(..)
    ) where

import           Data.Text (Text)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Glue ((/-/), (/|/), Glued, Glue)
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Lamdu.Prelude

type MainLoopEnv = MainLoop.Env

data WindowMode = FullScreen | Maximized

createWindow :: String -> WindowMode -> IO GLFW.Window
createWindow title mode =
    do
        monitor <- GLFWUtils.getPrimaryMonitor
        videoModeSize <- GLFWUtils.getVideoModeSize monitor
        let createWin = GLFWUtils.createWindow title
        case mode of
            FullScreen -> createWin (Just monitor) videoModeSize
            Maximized  -> createWin Nothing (videoModeSize - 1)

type WidgetId = Widget.Id
pattern WidgetId :: AnimId -> Widget.Id
pattern WidgetId animId = Widget.Id animId

parseMetaKey :: Text -> Maybe MetaKey
parseMetaKey = MetaKey.parse

formatMetaKey :: MetaKey -> Text
formatMetaKey = MetaKey.format

above :: Glue a b => a -> b -> Glued a b
above = (/-/)

leftOf :: Glue a b => a -> b -> Glued a b
leftOf = (/|/)
