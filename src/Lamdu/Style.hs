{-# LANGUAGE TemplateHaskell, NamedFieldPuns, OverloadedStrings, DisambiguateRecordFields #-}
module Lamdu.Style
    ( help
    , Style(..), style
    , mainLoopConfig

    , styleBase, styleAutoNameOrigin, styleNameOrigin
    , styleBytes, styleText, styleNum
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.Main.Animation (AnimConfig(..))
import           GUI.Momentu.MetaKey (MetaKey)
import           GUI.Momentu.Widget (CursorConfig(..))
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.Font as Fonts

data Style = Style
    { _styleBase :: TextEdit.Style
    , _styleAutoNameOrigin :: TextEdit.Style
    , _styleNameOrigin :: TextEdit.Style
    , _styleBytes :: TextEdit.Style
    , _styleText :: TextEdit.Style
    , _styleNum :: TextEdit.Style
    }
Lens.makeLenses ''Style

help :: Draw.Font -> [MetaKey] -> Theme.Help -> EventMapHelp.Config
help font helpKeys theme =
    EventMapHelp.Config
    { EventMapHelp._configStyle =
        TextView.Style
        { TextView._styleColor = helpTextColor
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , EventMapHelp._configInputDocColor = helpInputDocColor
    , EventMapHelp._configBGColor = helpBGColor
    , EventMapHelp._configOverlayDocKeys = helpKeys
    , EventMapHelp._configTint = helpTint
    }
    where
        Theme.Help{helpTextColor, helpInputDocColor, helpBGColor, helpTint} = theme

textEdit :: Draw.Color -> Draw.Font -> TextEdit.Style
textEdit color font =
    TextEdit.defaultStyle
    TextView.Style
    { TextView._styleColor = color
    , TextView._styleFont = font
    , TextView._styleUnderline = Nothing
    }

style :: Theme -> Fonts Draw.Font -> Style
style config fonts =
    Style
    { _styleBase =
      textEdit (Theme.baseColor config) (Fonts.fontDefault fonts)
    , _styleAutoNameOrigin =
      textEdit (Theme.baseColor config) (Fonts.fontAutoName fonts)
    , _styleNameOrigin =
      textEdit (Theme.baseColor config) (Fonts.fontBinders fonts)
    , _styleBytes =
      textEdit (Theme.literalColor config) (Fonts.fontLiteralBytes fonts)
    , _styleText =
      textEdit (Theme.literalColor config) (Fonts.fontLiteralText fonts)
    , _styleNum =
      textEdit (Theme.literalColor config) (Fonts.fontDefault fonts)
    }

mainLoopConfig :: Config -> Theme -> MainLoop.Config
mainLoopConfig config theme =
    MainLoop.Config
    { cAnim =
        AnimConfig
        { acTimePeriod = realToFrac (Theme.animationTimePeriodSec theme)
        , acRemainingRatioInPeriod = realToFrac (Theme.animationRemainInPeriod theme)
        }
    , cCursor =
        CursorConfig
        { cursorColor = Theme.cursorBGColor theme
        }
    , cZoom = Config.zoom config
    }
