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
    , EventMap.weakerEvents, EventMap.strongerEvents

    -- | Animations
    , AnimId

    -- | Element class
    , Element(..), Element.tint, Element.width, Element.height

    -- | Widget
    , Widget, Widget.EventResult
    , Widget.isFocused, widgetEvents
    , WidgetId, pattern WidgetId

    -- | View
    , View.unitSquare

    -- | GUI Layout
    , (/-/), (/|/), Glued
    , above -- ^ A named alias for `/-/`
    , leftOf -- ^ A named alias for `/|/`

    -- | Widget alignment
    , Align.Aligned(..), Align.alignmentRatio, Align.value
    , Align.WithTextPos(..), Align.textTop, Align.tValue

    -- | Drawing
    , MDraw.Font, MDraw.openFont, MDraw.Color(..)
    , MDraw.backgroundColor
    , MDraw.addInnerFrame

    -- | Environments
    , TextView.HasStyle(..)
    , Element.HasAnimIdPrefix(..)
    , Widget.HasCursor(..)
    , Widget.HasWidget(..)

    -- | Setup
    , GLFW.getPrimaryMonitor
    , GLFWUtils.getVideoModeSize
    , GLFWUtils.withGLFW
    , GLFWUtils.createWindow, GLFW.Window

    -- | Main loop
    , Zoom, Zoom.getZoomFactor
    , MainLoopEnv, MainLoop.eZoom, MainLoop.eWindowSize, MainLoop.quitEventMap
    , MainLoop.mainLoopWidget, MainLoop.defaultOptions

    -- | Basic types
    , Vector2(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Text (Text)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Element (Element(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Glue ((/-/), (/|/), Glued, Glue)
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

type MainLoopEnv = MainLoop.Env

type WidgetId = Widget.Id
pattern WidgetId :: AnimId -> Widget.Id
pattern WidgetId animId = Widget.Id animId

widgetEvents :: Widget.HasWidget w => Lens.Setter (w a) (w b) a b
widgetEvents = Widget.events

parseMetaKey :: Text -> Maybe MetaKey
parseMetaKey = MetaKey.parse

formatMetaKey :: MetaKey -> Text
formatMetaKey = MetaKey.format

above :: Glue a b => a -> b -> Glued a b
above x = (/-/) x

leftOf :: Glue a b => a -> b -> Glued a b
leftOf x = (/|/) x
