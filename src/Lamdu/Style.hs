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
import           Lamdu.Font (Fonts(..))
import qualified Lamdu.Font as Fonts

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

textEdit :: Draw.Color -> Font -> TextEdit.Style
textEdit color font =
    TextEdit.defaultStyle
    TextView.Style
    { TextView._styleColor = color
    , TextView._styleFont = font
    , TextView._styleUnderline = Nothing
    }

makeStyle :: Theme.TextColors -> Fonts Font -> Style
makeStyle config fonts =
    Style
    { _styleBase =
      textEdit (Theme.baseColor config) (Fonts.fontDefault fonts)
    , _styleAutoNameOrigin =
      textEdit (Theme.baseColor config) (Fonts.fontAutoName fonts)
    , _styleNameAtBinder =
      textEdit (Theme.baseColor config) (Fonts.fontBinders fonts)
    , _styleBytes =
      textEdit (Theme.literalColor config) (Fonts.fontLiteralBytes fonts)
    , _styleText =
      textEdit (Theme.literalColor config) (Fonts.fontLiteralText fonts)
    , _styleNum =
      textEdit (Theme.literalColor config) (Fonts.fontDefault fonts)
    }

mainLoopConfig :: Draw.R -> Font -> Config -> Theme -> MainLoop.Config
mainLoopConfig fontHeight helpFont config theme =
    MainLoop.Config
    { cAnim =
        AnimConfig
        { acTimePeriod = realToFrac (Theme.animationTimePeriodSec theme)
        , acRemainingRatioInPeriod = Theme.animationRemainInPeriod theme
        }
    , cCursor =
        Cursor.Config
        { cursorColor = Theme.cursorColor theme
        , Cursor.decay = Just Cursor.Decay
            { Cursor.heightUnit = fontHeight
            , Cursor.heightExponent = Theme.cursorDecayExponent theme
            }
        }
    , cZoom = config ^. Config.zoom
    , cHelpStyle = helpStyle helpFont (config ^. Config.helpKeys) (Theme.help theme)
    }
