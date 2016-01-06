{-# LANGUAGE RecordWildCards #-}
module Lamdu.Style
    ( flyNav
    , help
    , Style(..), style
    , anim
    ) where

import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.MainLoop (AnimConfig(..))
import           Graphics.UI.Bottle.SizedFont (SizedFont(..))
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.Font as Fonts
import qualified Lamdu.GUI.WidgetIds as WidgetIds

data Style = Style
    { styleBase :: TextEdit.Style
    , styleAutoNameOrigin :: TextEdit.Style
    , styleNameOrigin :: TextEdit.Style
    , styleBytes :: TextEdit.Style
    , styleText :: TextEdit.Style
    , styleNum :: TextEdit.Style
    }

flyNav :: FlyNav.Config
flyNav = FlyNav.Config
    { FlyNav.configLayer = -10000 -- that should cover it :-)
    }

help :: Draw.Font -> Config.Help -> EventMapDoc.Config
help font Config.Help{..} =
    EventMapDoc.Config
    { EventMapDoc.configStyle =
        TextView.Style
        { TextView._styleColor = helpTextColor
        , TextView._styleFont = SizedFont font helpTextSize
        , TextView._styleUnderline = Nothing
        }
    , EventMapDoc.configInputDocColor = helpInputDocColor
    , EventMapDoc.configBGColor = helpBGColor
    , EventMapDoc.configOverlayDocKeys = helpKeys
    }

textEdit :: Config -> Draw.Color -> Draw.Font -> TextEdit.Style
textEdit config color font =
    TextEdit.Style
    { TextEdit._sTextViewStyle =
      TextView.Style
      { TextView._styleColor = color
      , TextView._styleFont = SizedFont font (Config.baseTextSize config)
      , TextView._styleUnderline = Nothing
      }
    , TextEdit._sCursorColor = TextEdit.defaultCursorColor
    , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
    , TextEdit._sTextCursorId = WidgetIds.textCursorId
    , TextEdit._sBGColor = Config.cursorBGColor config
    , TextEdit._sEmptyUnfocusedString = ""
    , TextEdit._sEmptyFocusedString = ""
    }

style :: Config -> Fonts Draw.Font -> Style
style config fonts =
    Style
    { styleBase =
      textEdit config (Config.baseColor config) (Fonts.fontDefault fonts)
    , styleAutoNameOrigin =
      textEdit config nameOriginFGColor (Fonts.fontAutoName fonts)
    , styleNameOrigin =
      textEdit config nameOriginFGColor (Fonts.fontDefault fonts)
    , styleBytes =
      textEdit config (Config.literalColor config) (Fonts.fontMono fonts)
    , styleText =
      textEdit config (Config.literalColor config) (Fonts.fontFancy fonts)
    , styleNum =
      textEdit config (Config.literalColor config) (Fonts.fontDefault fonts)
    }
    where
        Config.Name{..} = Config.name config

anim :: Config -> AnimConfig
anim config =
    AnimConfig
    { acTimePeriod = realToFrac (Config.animationTimePeriodSec config)
    , acRemainingRatioInPeriod = realToFrac (Config.animationRemainInPeriod config)
    }
