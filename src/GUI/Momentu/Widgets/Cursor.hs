{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns #-}

-- | Render a widget and add a cursor to its frame (according to its focus area).
-- (This is only actually used in GUI.Momentu.Main)

module GUI.Momentu.Widgets.Cursor
    ( Config(..)
    , render
    ) where

import           Data.Vector.Vector2 (Vector2)
import qualified Data.Vector.Vector2 as Vector2
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction(..))
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
    , Maybe (Vector2 Widget.R -> Widget.EnterResult a)
    , Maybe (Rect, State.VirtualCursor -> EventMap a)
    )
render Config{cursorColor} w =
    case w ^. Widget.wState of
    Widget.StateUnfocused u ->
        -- Unfocused top level widget! TODO: Is this some sort of error?
        ( Element.render (u ^. Widget.uLayers)
        , u ^. Widget.uMEnter <&> (. Point)
        , Nothing
        )
    Widget.StateFocused f ->
        ( cursorFrame <> Element.render (r ^. Widget.fLayers)
        , r ^. Widget.fMEnterPoint
        , Just (area, mkEventMap)
        )
        where
            r = f (Widget.Surrounding 0 0 0 0)
            mkEventMap x =
                (r ^. Widget.fEventMap)
                Widget.EventContext
                { Widget._eVirtualCursor = x
                , Widget._ePrevTextRemainder = mempty
                }
            area = last (r ^. Widget.fFocalAreas)
            frameWidth = 5
            opacity = 0.5 * max 0 (1 - 40 * Vector2.uncurry (*) (area ^. Rect.size / w ^. Widget.wSize))
            cursorFrame =
                ( Anim.backgroundColor ["cursor-background"] cursorColor (area ^. Rect.size - frameWidth * 2)
                    & Anim.translate frameWidth
                    & Anim.unitImages %~ Draw.tint (Draw.Color 1 1 1 opacity)
                )
                <>
                ( Anim.emptyRectangle frameWidth (area ^. Rect.size) ["cursor-frame"]
                    & Anim.unitImages %~ Draw.tint cursorColor
                    & Anim.unitImages %~ Draw.tint (Draw.Color 1 1 1 (1 - opacity))
                )
                & Anim.translate (area ^. Rect.topLeft)
