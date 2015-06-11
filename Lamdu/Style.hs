{-# LANGUAGE RecordWildCards #-}
module Lamdu.Style
    ( flyNav
    , help
    , base
    , IsDebugMode, anim
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
import qualified Lamdu.GUI.WidgetIds as WidgetIds

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
        }
    , EventMapDoc.configInputDocColor = helpInputDocColor
    , EventMapDoc.configBGColor = helpBGColor
    , EventMapDoc.configOverlayDocKeys = helpKeys
    }

base :: Config -> Draw.Font -> TextEdit.Style
base config font = TextEdit.Style
    { TextEdit._sTextViewStyle =
        TextView.Style
        { TextView._styleColor = Config.baseColor config
        , TextView._styleFont = SizedFont font (Config.baseTextSize config)
        }
    , TextEdit._sCursorColor = TextEdit.defaultCursorColor
    , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
    , TextEdit._sTextCursorId = WidgetIds.textCursorId
    , TextEdit._sBGColor = Config.cursorBGColor config
    , TextEdit._sEmptyUnfocusedString = ""
    , TextEdit._sEmptyFocusedString = ""
    }

-- TODO: Kill debug mode?
type IsDebugMode = Bool
anim :: Config -> IsDebugMode -> AnimConfig
anim _ True =
    AnimConfig
    { acTimePeriod = 6.64
    , acRemainingRatioInPeriod = 0.01
    }
anim config False =
    AnimConfig
    { acTimePeriod = realToFrac (Config.animationTimePeriodSec config)
    , acRemainingRatioInPeriod = realToFrac (Config.animationRemainInPeriod config)
    }
