{-# LANGUAGE TemplateHaskell, NamedFieldPuns, DisambiguateRecordFields #-}
module Lamdu.Style
    ( Style(..), makeStyle
    , HasStyle(..)
    , mainLoopConfig

    , styleBase, styleAutoNameOrigin, styleNameAtBinder
    , styleBytes, styleText, styleNum
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.Main.Animation (AnimConfig(..))
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors(..))
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.Font as Font

import           Lamdu.Prelude

data Style = Style
    { _styleBase :: TextEdit.Style
    , _styleAutoNameOrigin :: TextEdit.Style
    , _styleNameAtBinder :: TextEdit.Style
    , _styleBytes :: TextEdit.Style
    , _styleText :: TextEdit.Style
    , _styleNum :: TextEdit.Style
    }
Lens.makeLenses ''Style

class TextEdit.HasStyle env => HasStyle env where style :: Lens' env Style

helpStyle :: Font -> [MetaKey] -> Theme.Help -> EventMapHelp.Config
helpStyle font helpKeys theme =
    EventMapHelp.Config
    { EventMapHelp._configStyle =
        TextView.Style
        { TextView._styleColor = theme ^. Theme.helpTextColor
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , EventMapHelp._configInputDocColor = theme ^. Theme.helpInputDocColor
    , EventMapHelp._configBGColor = theme ^. Theme.helpBGColor
    , EventMapHelp._configOverlayDocKeys = helpKeys
    , EventMapHelp._configTint = theme ^. Theme.helpTint
    }

textEdit :: Draw.Color -> Font -> TextEdit.Style
textEdit color font =
    TextEdit.defaultStyle
    TextView.Style
    { TextView._styleColor = color
    , TextView._styleFont = font
    , TextView._styleUnderline = Nothing
    }

makeStyle :: TextColors -> Fonts Font -> Style
makeStyle config fonts =
    Style
    { _styleBase =
      textEdit (TextColors.baseColor config) (fonts ^. Font.fontDefault)
    , _styleAutoNameOrigin =
      textEdit (TextColors.baseColor config) (fonts ^. Font.fontAutoName)
    , _styleNameAtBinder =
      textEdit (TextColors.baseColor config) (fonts ^. Font.fontBinders)
    , _styleBytes =
      textEdit (TextColors.literalColor config) (fonts ^. Font.fontLiteralBytes)
    , _styleText =
      textEdit (TextColors.literalColor config) (fonts ^. Font.fontLiteralText)
    , _styleNum =
      textEdit (TextColors.literalColor config) (fonts ^. Font.fontDefault)
    }

mainLoopConfig :: Draw.R -> Font -> Config -> Theme -> MainLoop.Config
mainLoopConfig fontHeight helpFont config theme =
    MainLoop.Config
    { cAnim =
        AnimConfig
        { acTimePeriod = theme ^. Theme.animationTimePeriodSec & realToFrac
        , acRemainingRatioInPeriod = theme ^. Theme.animationRemainInPeriod
        }
    , cCursor =
        Cursor.Config
        { cursorColor = theme ^. Theme.cursorColor
        , Cursor.decay = Just Cursor.Decay
            { Cursor.heightUnit = fontHeight
            , Cursor.heightExponent = theme ^. Theme.cursorDecayExponent
            }
        }
    , cZoom = config ^. Config.zoom
    , cHelpStyle = helpStyle helpFont (config ^. Config.helpKeys) (theme ^. Theme.help)
    }
