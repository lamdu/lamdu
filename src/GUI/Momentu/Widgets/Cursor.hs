{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NamedFieldPuns #-}

-- | Render a widget and add a cursor to its frame (according to its focus area).
-- (This is only actually used in GUI.Momentu.Main)

module GUI.Momentu.Widgets.Cursor
    ( Config(..), Decay(..)
    , render
    ) where

import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction(..))
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget

import           Lamdu.Prelude

data Decay = Decay
    { heightUnit :: Draw.R
    , heightExponent :: Draw.R
    }

data Config = Config
    { cursorColor :: Draw.Color
    , decay :: Maybe Decay
    }

render ::
    Config -> Widget a ->
    ( Anim.Frame
    , Maybe (Vector2 Widget.R -> Widget.EnterResult a)
    , Maybe (Rect, State.VirtualCursor -> EventMap a)
    )
render Config{cursorColor, decay} w =
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
            areaSize = area ^. Rect.width * area ^. Rect.height
            color =
                case decay of
                Nothing -> cursorColor
                Just (Decay unit power) ->
                    cursorColor
                    & Draw.alphaChannel //~ (areaSize / unit**2) ** power
            cursorFrame =
                Anim.coloredRectangle ["cursor"] color (area ^. Rect.size)
                & Anim.translate (area ^. Rect.topLeft)
