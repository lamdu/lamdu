{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns #-}

-- | Render a widget and add a cursor to its frame (according to its focus area).
-- (This is only actually used in GUI.Momentu.Main)

module GUI.Momentu.Widgets.Cursor
    ( Config(..)
    , render
    ) where

import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

newtype Config = Config
    { cursorColor :: Draw.Color
    }

render ::
    Config -> Widget a ->
    ( Anim.Frame
    , Maybe (Direction -> Widget.EnterResult a)
    , Maybe (Rect, State.VirtualCursor -> EventMap a)
    )
render Config{cursorColor} w =
    case w ^. Widget.wState of
    Widget.StateUnfocused u ->
        -- Unfocused top level widget! TODO: Is this some sort of error?
        ( Element.render (u ^. Widget.uLayers)
        , u ^. Widget.uMEnter
        , Nothing
        )
    Widget.StateFocused f ->
        ( cursorFrame <> Element.render (r ^. Widget.fLayers)
        , r ^. Widget.fMEnter
        , Just (area, r ^. Widget.fEventMap)
        )
        where
            r = f (Widget.Surrounding 0 0 0 0)
            area = last (r ^. Widget.fFocalAreas)
            cursorFrame =
                Anim.backgroundColor ["cursor-background"] cursorColor (area ^. Rect.size)
                & Anim.translate (area ^. Rect.topLeft)
