{-# LANGUAGE NamedFieldPuns #-}

-- | Render a widget and add a cursor to its frame (according to its focus area).
-- (This is only actually used in GUI.Momentu.Main)

module GUI.Momentu.Widgets.Cursor
    ( Config(..), Decay(..)
    , render
    ) where

import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget

import           GUI.Momentu.Prelude

data Decay = Decay
    { heightUnit :: Draw.R
    , heightExponent :: Draw.R
    }

data Config = Config
    { cursorColor :: Draw.Color
    , decay :: Maybe Decay
    }

render ::
    Widget f ->
    ( Config -> Anim.Frame
    , Maybe (Vector2 Widget.R -> Widget.EnterResult (f State.Update))
    , Maybe (Rect, State.VirtualCursor -> EventMap (f State.Update))
    )
render w =
    case w ^. Widget.wState of
    Widget.StateUnfocused u ->
        -- Unfocused top level widget! TODO: Is this some sort of error?
        ( const $ Element.render (u ^. Widget.uLayers)
        , u ^. Widget.uMEnter <&> (. Point)
        , Nothing
        )
    Widget.StateFocused f ->
        ( \config -> cursorFrame config <> Element.render (r ^. Widget.fLayers)
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
            color Config{cursorColor, decay} =
                case decay of
                Nothing -> cursorColor
                Just (Decay unit power) ->
                    cursorColor
                    & Draw.alphaChannel //~ (areaSize / unit**2) ** power
            cursorFrame config =
                Anim.coloredRectangle ["cursor"] (color config)
                & Anim.scale (area ^. Rect.size)
                & Anim.translate (area ^. Rect.topLeft)
