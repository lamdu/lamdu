{-# LANGUAGE TemplateHaskell, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.Style
    ( Style(..), base, autoNameOrigin, nameAtBinder, bytes, text, num, help
    , make
    , HasStyle(..)
    , FontInfo(..)
    , mainLoopConfig
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.Main.Animation (AnimConfig(..))
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           GUI.Momentu.Zoom (Zoom)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (Fonts)
import qualified Lamdu.Config.Theme.Fonts as Fonts
import qualified Lamdu.Config.Theme.TextColors as TextColors

import           Lamdu.Prelude

data Style = Style
    { _base :: TextEdit.Style
    , _autoNameOrigin :: TextEdit.Style
    , _nameAtBinder :: TextEdit.Style
    , _bytes :: TextEdit.Style
    , _text :: TextEdit.Style
    , _num :: TextEdit.Style
    , _help :: EventMapHelp.Style
    }
Lens.makeLenses ''Style

class TextEdit.HasStyle env => HasStyle env where style :: Lens' env Style

helpStyle :: Font -> Theme.Help -> EventMapHelp.Style
helpStyle font theme =
    EventMapHelp.Style
    { EventMapHelp._styleText =
        TextView.Style
        { TextView._styleColor = theme ^. Theme.helpTextColor
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , EventMapHelp._styleInputDocColor = theme ^. Theme.helpInputDocColor
    , EventMapHelp._styleBGColor = theme ^. Theme.helpBGColor
    , EventMapHelp._styleTint = theme ^. Theme.helpTint
    }

make :: Fonts Font -> Theme -> Style
make fonts theme =
    Style
    { _base           = textEdit TextColors.baseColor    Fonts.base
    , _autoNameOrigin = textEdit TextColors.baseColor    Fonts.autoName
    , _nameAtBinder   = textEdit TextColors.baseColor    Fonts.binders
    , _bytes          = textEdit TextColors.literalColor Fonts.literalBytes
    , _text           = textEdit TextColors.literalColor Fonts.literalText
    , _num            = textEdit TextColors.literalColor Fonts.base
    , _help           = helpStyle (fonts ^. Fonts.help) (theme ^. Theme.help)
    }
    where
        textEdit color font =
            TextEdit.defaultStyle
            TextView.Style
            { TextView._styleColor = theme ^. Theme.textColors . color
            , TextView._styleFont = fonts ^. font
            , TextView._styleUnderline = Nothing
            }

newtype FontInfo = FontInfo { fontHeight :: Draw.R }

mainLoopConfig :: (Zoom -> IO FontInfo) -> IO (Config, Theme) -> MainLoop.Config
mainLoopConfig getFontInfo getConfig =
    MainLoop.Config
    { cAnim =
        getConfig
        <&> \(_config, theme) ->
        AnimConfig
        { acTimePeriod = theme ^. Theme.animationTimePeriodSec & realToFrac
        , acRemainingRatioInPeriod = theme ^. Theme.animationRemainInPeriod
        }
    , cCursor =
        \zoom ->
        (,) <$> getFontInfo zoom <*> getConfig
        <&> \(fi, (_config, theme)) ->
        Cursor.Config
        { cursorColor = theme ^. Theme.cursorColor
        , Cursor.decay = Just Cursor.Decay
            { Cursor.heightUnit = fontHeight fi
            , Cursor.heightExponent = theme ^. Theme.cursorDecayExponent
            }
        }
    , cZoom = getConfig <&> (^. _1 . Config.zoom)
    , cHelpEnv = Nothing
    , cInvalidCursorOverlayColor =
        getConfig
        <&> \(_config, theme) ->
        theme ^. Theme.invalidCursorOverlayColor
    }
