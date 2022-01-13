-- | Make Lamdu Style
{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Lamdu.Style.Make
    ( make
    , mainLoopConfig
    ) where

import qualified Control.Lens as Lens
import           Data.Property (MkProperty)
import qualified Data.Property as Property
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Main.Animation as Anim
import qualified GUI.Momentu.Main.Config as MainConfig
import           GUI.Momentu.ModKey (ModKey)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
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
import           Lamdu.I18N.Language (Language)
import           Lamdu.Style (Style(..))

import           Lamdu.Prelude

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
    , EventMapHelp._styleSrcLocColor = theme ^. Theme.helpSrcLocColor
    }

make :: Fonts Font -> Theme -> Style
make fonts theme =
    Style
    { _base           = textEdit TextColors.baseColor    Fonts.base
    , _autoNameOrigin = textEdit TextColors.baseColor    Fonts.autoName
    , _nameAtBinder   = textEdit TextColors.baseColor    Fonts.binders
    , _bytes          = textEdit TextColors.literalColor Fonts.literalBytes
    , _text           = textEdit TextColors.literalColor Fonts.literalText
    , _char           = textEdit TextColors.literalColor Fonts.literalText
    , _num            = textEdit TextColors.literalColor Fonts.base
    }
    where
        textEdit color font =
            TextEdit.Style
            { TextEdit._sCursorColor = theme ^. Theme.textEditCursorColor
            , TextEdit._sCursorWidth = theme ^. Theme.textEditCursorWidth
            , TextEdit._sEmptyStringsColors =
                theme ^. Theme.textColors . TextColors.emptyEditColors
            , TextEdit._sTextViewStyle =
                TextView.Style
                { TextView._styleColor = theme ^. Theme.textColors . color
                , TextView._styleFont = fonts ^. font
                , TextView._styleUnderline = Nothing
                }
            }

data HelpEnv = HelpEnv
    { _heConfig :: !EventMapHelp.Config
    , _heStyle :: !EventMapHelp.Style
    , _heAnimIdPrefix :: !AnimId
    , _heDirLayout :: !Dir.Layout
    , _heDirTexts :: !(Dir.Texts Text)
    , _heGlueTexts :: !(Glue.Texts Text)
    , _heCommonTexts :: !(MomentuTexts.Texts Text)
    , _heEventMapTexts :: !(E.Texts Text)
    }
Lens.makeLenses ''HelpEnv
instance Element.HasAnimIdPrefix HelpEnv where animIdPrefix = heAnimIdPrefix
instance Has TextView.Style HelpEnv where has = heStyle . has
instance Has Dir.Layout HelpEnv where has = heDirLayout
instance Has (Dir.Texts Text) HelpEnv where has = heDirTexts
instance Has (Glue.Texts Text) HelpEnv where has = heGlueTexts
instance Has (E.Texts Text) HelpEnv where has = heEventMapTexts
instance Has EventMapHelp.Config HelpEnv where has = heConfig
instance Has EventMapHelp.Style HelpEnv where has = heStyle
instance Has (MomentuTexts.Texts Text) HelpEnv where has = heCommonTexts

addHelp ::
    Config ModKey -> Theme -> Language -> Font ->
    Widget.Size -> Widget f -> Widget f
addHelp config theme language font size widget =
    widget
    & Widget.wState . Widget._StateFocused . Lens.mapped %~
    (EventMapHelp.addHelpView size ?? env)
    where
        env =
            HelpEnv
            { _heConfig =
                EventMapHelp.Config
                { EventMapHelp._configOverlayDocKeys = config ^. Config.helpKeys
                }
            , _heAnimIdPrefix = ["help box"]
            , _heDirLayout = language ^. has
            , _heDirTexts = language ^. has
            , _heEventMapTexts = language ^. has
            , _heGlueTexts = language ^. has
            , _heStyle = helpStyle font (theme ^. Theme.help)
            , _heCommonTexts = language ^. has
            }

mainLoopConfig ::
    MkProperty IO o EventMapHelp.IsHelpShown ->
    (Zoom -> IO (Fonts Font)) -> IO (Config ModKey, Theme, Language) -> MainConfig.Config
mainLoopConfig helpProp getFonts getConfig =
    MainConfig.Config
    { _cAnim =
        getConfig <&> (^. _2)
        <&> \theme ->
        Anim.Config
        { _acTimePeriod = theme ^. Theme.animationTimePeriodSec & realToFrac
        , _acRemainingRatioInPeriod = theme ^. Theme.animationRemainInPeriod
        , _acSpiral = Anim.SpiralAnimConf 0 0
        }
    , _cCursor =
        \zoom ->
        (,) <$> getFonts zoom <*> (getConfig <&> (^. _2))
        <&> \(fonts, theme) ->
        Cursor.Config
        { cursorColor = theme ^. Theme.cursorColor
        , Cursor.decay = Just Cursor.Decay
            { Cursor.heightUnit = fonts ^. Fonts.base & Font.height
            , Cursor.heightExponent = theme ^. Theme.cursorDecayExponent
            }
        }
    , _cZoom = getConfig <&> (^. _1 . Config.zoom)
    , _cPostProcess =
        \zoom size widget ->
        Property.getP helpProp
        >>= \case
        HelpNotShown -> pure widget
        HelpShown ->
            do
                helpFont <- getFonts zoom <&> (^. Fonts.help)
                (config, theme, language) <- getConfig
                addHelp config theme language helpFont size widget & pure
    , _cInvalidCursorOverlayColor =
        getConfig <&> (^. _2)
        <&> \theme ->
        theme ^. Theme.invalidCursorOverlayColor
    }
