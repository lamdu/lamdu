{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
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

help :: Draw.Font -> Config.Help -> EventMapDoc.Config
help font Config.Help{..} =
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

style :: Config -> Fonts Draw.Font -> Style
style config fonts =
    Style
    { styleBase =
      textEdit (Config.baseColor config) (Fonts.fontDefault fonts)
    , styleAutoNameOrigin =
      textEdit (Config.baseColor config) (Fonts.fontAutoName fonts)
    , styleNameOrigin =
      textEdit (Config.baseColor config) (Fonts.fontBinders fonts)
    , styleBytes =
      textEdit (Config.literalColor config) (Fonts.fontLiteralBytes fonts)
    , styleText =
      textEdit (Config.literalColor config) (Fonts.fontLiteralText fonts)
    , styleNum =
      textEdit (Config.literalColor config) (Fonts.fontDefault fonts)
    }
    where
        Config.Name{..} = Config.name config

mainLoopConfig :: Config -> MainLoop.Config
mainLoopConfig config =
    MainLoop.Config
    { cAnim =
        AnimConfig
        { acTimePeriod = realToFrac (Config.animationTimePeriodSec config)
        , acRemainingRatioInPeriod = realToFrac (Config.animationRemainInPeriod config)
        }
    , cCursor =
        CursorConfig
        { cursorColor = Config.cursorBGColor config
        }
    }
