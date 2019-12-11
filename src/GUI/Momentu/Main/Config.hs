-- | The types used for the mainloop
{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Main.Config
    ( Config(..), cAnim, cCursor, cZoom, cPostProcess, cInvalidCursorOverlayColor
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Animation.Engine as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom

import           GUI.Momentu.Prelude

data Config = Config
    { _cAnim :: IO Anim.Config
    , _cCursor :: Zoom -> IO Cursor.Config
    , _cZoom :: IO Zoom.Config
    , _cPostProcess :: Zoom -> Widget.Size -> Widget IO -> IO (Widget IO)
    , _cInvalidCursorOverlayColor :: IO Draw.Color
    }
Lens.makeLenses ''Config
