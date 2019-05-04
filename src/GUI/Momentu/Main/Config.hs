-- | The types used for the mainloop
{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Main.Config
    ( Config(..), cAnim, cCursor, cZoom, cHelpEnv, cInvalidCursorOverlayColor
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Animation.Engine as Anim
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom

import           Lamdu.Prelude

data Config = Config
    { _cAnim :: IO Anim.Config
    , _cCursor :: Zoom -> IO Cursor.Config
    , _cZoom :: IO Zoom.Config
    , _cHelpEnv :: Maybe (Zoom -> IO EventMapHelp.Env)
    , _cInvalidCursorOverlayColor :: IO Draw.Color
    }
Lens.makeLenses ''Config
