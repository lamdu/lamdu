{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Lamdu.Style
    ( help
    , Style(..), style
    , mainLoopConfig
    ) where

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Main as MainLoop
import           Graphics.UI.Bottle.Main.Animation (AnimConfig(..))
import           Graphics.UI.Bottle.Widget (CursorConfig(..))
import qualified Graphics.UI.Bottle.Widgets.EventMapHelp as EventMapHelp
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.Font as Fonts

data Style = Style
    { styleBase :: TextEdit.Style
    , styleAutoNameOrigin :: TextEdit.Style
    , styleNameOrigin :: TextEdit.Style
    , styleBytes :: TextEdit.Style
    , styleText :: TextEdit.Style
    , styleNum :: TextEdit.Style
    }

help :: Draw.Font -> Config.Help -> Theme.Help -> EventMapHelp.Config
help font config theme =
    EventMapHelp.Config
    { EventMapHelp.configStyle =
        TextView.Style
        { TextView._styleColor = helpTextColor
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , EventMapHelp.configInputDocColor = helpInputDocColor
    , EventMapHelp.configBGColor = helpBGColor
    , EventMapHelp.configOverlayDocKeys = helpKeys
    , EventMapHelp.configTint = helpTint
    }
    where
        Config.Help{helpKeys} = config
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
    { styleBase =
      textEdit (Theme.baseColor config) (Fonts.fontDefault fonts)
    , styleAutoNameOrigin =
      textEdit (Theme.baseColor config) (Fonts.fontAutoName fonts)
    , styleNameOrigin =
      textEdit (Theme.baseColor config) (Fonts.fontBinders fonts)
    , styleBytes =
      textEdit (Theme.literalColor config) (Fonts.fontLiteralBytes fonts)
    , styleText =
      textEdit (Theme.literalColor config) (Fonts.fontLiteralText fonts)
    , styleNum =
      textEdit (Theme.literalColor config) (Fonts.fontDefault fonts)
    }
    where
        Theme.Name{..} = Theme.name config

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
