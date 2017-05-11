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
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
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

help :: Draw.Font -> Config.Help -> Theme.Help -> EventMapDoc.Config
help font config theme =
    EventMapDoc.Config
    { EventMapDoc.configStyle =
        TextView.Style
        { TextView._styleColor = helpTextColor
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , EventMapDoc.configInputDocColor = helpInputDocColor
    , EventMapDoc.configBGColor = helpBGColor
    , EventMapDoc.configOverlayDocKeys = helpKeys
    , EventMapDoc.configTint = helpTint
    }
    where
        Config.Help{helpKeys} = config
        Theme.Help{helpTextColor, helpInputDocColor, helpBGColor, helpTint} = theme

textEdit :: Draw.Color -> Draw.Font -> TextEdit.Style
textEdit color font =
    TextEdit.Style
    { TextEdit._sTextViewStyle =
      TextView.Style
      { TextView._styleColor = color
      , TextView._styleFont = font
      , TextView._styleUnderline = Nothing
      }
    , TextEdit._sCursorColor = TextEdit.defaultCursorColor
    , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
    , TextEdit._sEmptyUnfocusedString = ""
    , TextEdit._sEmptyFocusedString = ""
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

mainLoopConfig :: Theme -> MainLoop.Config
mainLoopConfig theme =
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
    }
