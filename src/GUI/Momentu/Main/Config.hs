-- | The types used for the mainloop

module GUI.Momentu.Main.Config
    ( Config(..)
    ) where

import qualified GUI.Momentu.Animation.Engine as Anim
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom

import           Lamdu.Prelude

data Config = Config
    { cAnim :: IO Anim.Config
    , cCursor :: Zoom -> IO Cursor.Config
    , cZoom :: IO Zoom.Config
    , cHelpEnv :: Maybe (Zoom -> IO EventMapHelp.Env)
    , cInvalidCursorOverlayColor :: IO Draw.Color
    }
